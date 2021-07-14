!
! rwlock_module.f90 -- readers-writers lock with OpenMP
!
! Patrick J. Fasano
! University of Notre Dame, Lawrence Berkeley National Laboratory
!
! SPDX-License-Identifier: BSD-3-Clause
!
module rwlock
    !! A module implementing a readers-writer lock in OpenMP.

    use omp_lib
    implicit none

    type ReadWriteLock_t
        private
        integer :: num_readers=0, waiting_readers=0
        integer(kind=omp_lock_kind) :: w_lock
    contains
        procedure :: init => rwlock_init
        procedure :: reader_lock => rwlock_reader_lock
        procedure :: reader_unlock => rwlock_reader_unlock
        procedure :: writer_lock => rwlock_writer_lock
        procedure :: writer_unlock => rwlock_writer_unlock
    end type ReadWriteLock_t

    public :: ReadWriteLock_t
    private :: rwlock_init
    private :: rwlock_reader_lock, rwlock_reader_unlock
    private :: rwlock_writer_lock, rwlock_writer_unlock
    integer, private :: nnn
    integer, parameter, private :: max_readers = huge(nnn)

contains

    subroutine rwlock_init(this)
        !! Initialize the readers-writers lock.
        implicit none
        class(ReadWriteLock_t), intent(inout) :: this
        call omp_init_lock(this%w_lock)
    end subroutine rwlock_init

    subroutine rwlock_reader_lock(this)
        !! Acquire a reader lock.
        !!
        !! This subroutine will spin until a lock is acquired successfully.
        implicit none
        class(ReadWriteLock_t), intent(inout) :: this
        integer :: r

        !$omp atomic capture seq_cst
        this%num_readers = this%num_readers + 1
        r = this%num_readers
        !$omp end atomic

        ! spin while r < 0 -- writer acquiring or acquired lock
        if (r .lt. 0) then
            do while (r .lt. 0)
                !$omp atomic read seq_cst
                r = this%num_readers
                !$omp end atomic
            end do

            ! after we're done spinning, decrement the number of waiting readers
            ! to signal that we've left the spin loop
            !$omp atomic update seq_cst
            this%waiting_readers = this%waiting_readers - 1
            !$omp end atomic

            ! do an acquire to make sure we get updated versions of
            ! protected shared data
            !$omp flush acquire
        end if
    end subroutine rwlock_reader_lock

    subroutine rwlock_reader_unlock(this)
        !! Release a reader lock.
        implicit none
        class(ReadWriteLock_t), intent(inout) :: this
        integer :: r

        !$omp atomic capture seq_cst
        r = this%num_readers
        this%num_readers = this%num_readers - 1
        !$omp end atomic

        ! if r < 0, a writer is trying to acquire a lock -- signal that we've
        ! released the reader lock
        if (r .lt. 0) then
            !$omp atomic update seq_cst
            this%waiting_readers = this%waiting_readers - 1
            !$omp end atomic
        end if
    end subroutine rwlock_reader_unlock

    function rwlock_writer_lock(this, upgrade) result(stat)
        !! Acquire the writer lock.
        !!
        !! This function will spin while waiting for existing readers to unlock.
        !!
        !! This is non-blocking with respect to other writer locks. That is, if
        !! another writer is acquiring/has acquired a writer lock, this will
        !! return .false. -- if this occurs, most likely, the calling code will
        !! want to acquire a reader lock, check that writing is still necessary,
        !! and then (possibly) try again to acquire the writer lock.
        implicit none
        class(ReadWriteLock_t), intent(inout) :: this
        logical, intent(in) :: upgrade !! upgrade a reader lock to a writer lock
        logical :: stat !! Lock acquisition success status
        integer :: r, f

        ! try to get the writer lock, return immediately if we can't
        stat = omp_test_lock(this%w_lock)
        if (.not. stat) return

        ! release the reader lock if we're upgrading to a writer lock
        if (upgrade) call this%reader_unlock()

        ! subtract max_readers from num_readers so that negative value signals
        ! to readers that we want a writer lock
        !$omp atomic capture seq_cst
        r = this%num_readers
        this%num_readers = this%num_readers - max_readers
        !$omp end atomic

        ! if readers are executing when we set num_readers to be negative,
        ! spin until that number of readers have finished
        if (r .gt. 0) then
            !$omp atomic capture seq_cst
            this%waiting_readers = this%waiting_readers + r
            f = this%waiting_readers
            !$omp end atomic
            do while (f .ne. 0)
                !$omp atomic read seq_cst
                f = this%waiting_readers
                !$omp end atomic
            end do
        end if

        ! do an acquire to make sure we get updated versions of
        ! protected shared data
        !$omp flush acquire

        return

    end function rwlock_writer_lock

    subroutine rwlock_writer_unlock(this, downgrade)
        !! Release the writer lock
        implicit none
        class(ReadWriteLock_t), intent(inout) :: this
        logical, intent(in) :: downgrade !! downgrade writer lock to reader lock
        integer :: r, f

        ! do a release to make sure we publish updated versions of
        ! protected shared data
        !$omp flush release

        ! add max_readers to num_readers to signal that readers can continue
        !$omp atomic capture seq_cst
        this%num_readers = this%num_readers + max_readers
        r = this%num_readers
        !$omp end atomic

        ! if readers are spinning when we set num_readers to be positive,
        ! spin until that number of readers have been released
        if (r .gt. 0) then
            !$omp atomic capture seq_cst
            this%waiting_readers = this%waiting_readers + r
            f = this%waiting_readers
            !$omp end atomic
            do while (f .ne. 0)
                !$omp atomic read seq_cst
                f = this%waiting_readers
                !$omp end atomic
            end do
        end if

        ! reacquire a reader lock if we're downgrading from a writer lock
        if (downgrade) call this%reader_lock()

        call omp_unset_lock(this%w_lock)

        return

    end subroutine rwlock_writer_unlock

end module rwlock
