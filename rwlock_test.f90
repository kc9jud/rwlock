!
! rwlock_test.f90 -- simple test using readers-writers lock with OpenMP
!
! Patrick J. Fasano
! University of Notre Dame, Lawrence Berkeley National Laboratory
!
! SPDX-License-Identifier: BSD-3-Clause
!
program rwlock_test
use omp_lib
use rwlock
implicit none

integer :: i, thread
integer(kind=4) :: s=16, t
integer(kind=4), dimension (:), allocatable :: arr, temp
logical :: got_wlock
type(ReadWriteLock_t) :: lock

allocate(arr(s))

arr(:) = 0

! initialize the lock
call lock%init()

!$omp parallel do schedule(static) default(none) &
!$omp     private(thread, t) &
!$omp     shared(lock, s, arr, temp)
do i = 1, 512

    thread = omp_get_thread_num()

    ! acquire a reader lock
    call lock%reader_lock()

    ! if we need to resize `arr`, try to get a writer lock
    do while (s .lt. i)
        if (.not. lock%writer_lock(.true.)) then
            ! if we didn't get the writer lock, some other thread is resizing
            ! the array -- wait for it to finish, and re-check if resizing is
            ! still required
            call lock%reader_unlock()
            call lock%reader_lock()
            ! flush, to get update from other thread which did update
            !$omp flush acquire
            cycle
        end if
        ! flush, to get any updates from other writers
        !$omp flush acquire
        print*, thread, "write lock", i, s

        ! resize the array
        t = ceiling(i/16.)*16
        allocate(temp(t))
        temp(1:s) = arr(1:s)
        temp(s+1:t) = -1
        call move_alloc(temp, arr)
        s = t
        print*, thread, "resize", i, s
        ! flush, to publish update to other threads
        !$omp flush release

        ! release the writer lock
        call lock%writer_unlock(.true.)
        print*, thread, "write unlock", i, s
    end do

    arr(i) = s

    ! release the reader lock
    call lock%reader_unlock()

end do
!$omp end parallel do

print*, s
print*, arr

deallocate(arr)

end program
