module memory_printer
  use spmdMod        , only: masterproc, mpicom
  use clm_varctl     , only: iulog
  use shr_kind_mod        , only : r8 => shr_kind_r8
  use shr_mem_mod, only : shr_mem_getusage
  use shr_mpi_mod, only : shr_mpi_min, shr_mpi_max
  use shr_sys_mod, only : shr_sys_flush

  implicit none
  private

  public :: print_memory_usage

contains

  subroutine print_memory_usage(message)
    character(len=*), intent(in) :: message
    logical :: doprint
    real(r8) :: msize, mrss
    real(r8) :: msize0, msize1, mrss0, mrss1

    character(len=*), parameter :: subname = 'ctsm_print_memory_usage'
    if ( masterproc ) then
       doprint = .true.
    else
       doprint = .false.
    end if

    call shr_mem_getusage(msize,mrss,doprint)
    if (doprint) then
       write(iulog,*) '----- ', message, ' -----'
       write(iulog,*) subname, ' my memory highwater  (MB)      = ', msize
       write(iulog,*) subname, ' my memory last usage (MB)      = ', mrss
    end if

    call shr_mpi_min(msize ,msize0,mpicom,' ctsm msize0', all=.true.)
    call shr_mpi_max(msize ,msize1,mpicom,' ctsm msize1', all=.true.)
    call shr_mpi_min(mrss  ,mrss0,mpicom,'  ctsm mrss0',  all=.true.)
    call shr_mpi_max(mrss  ,mrss1,mpicom,'  ctsm mrss1',  all=.true.)
    if (masterproc) then
       write(iulog,*) subname,' pes min memory highwater  (MB)  = ',msize0
       write(iulog,*) subname,' pes max memory highwater  (MB)  = ',msize1
       write(iulog,*) subname,' pes min memory last usage (MB)  = ',mrss0
       write(iulog,*) subname,' pes max memory last usage (MB)  = ',mrss1
    end if
    if (doprint) then
       write(iulog,*) '--------------------------------------------------'
       call shr_sys_flush(iulog)
    end if
  end subroutine print_memory_usage

end module memory_printer
