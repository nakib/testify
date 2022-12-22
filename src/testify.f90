module testify_m

  use iso_fortran_env, only : &
       i8 => int8, i16 => int16, i32 => int32, i64 => int64, &
       r32 => real32, r64 => real64, r128 => real128
  
  implicit none

  public testify
  
  type testify
     character(:), private, allocatable :: name
     logical, private :: status = .false.
     integer, private :: test_count = 0
     integer, private :: pass_count = 0
     
   contains
     procedure, public :: set_name, report
     generic,  public :: assert => assert_scalar, assert_array
     procedure, private :: assert_scalar, assert_array, assert_elemental
  end type testify
 
contains

  subroutine set_name(self, name)
    class(testify), intent(inout) :: self
    character(*), intent(in) :: name

    self%name = name
  end subroutine set_name
    
  subroutine assert_scalar(self, val, ref)
    class(testify), intent(inout) :: self
    class(*), intent(in) :: val, ref

    self%test_count = self%test_count + 1
    
    self%status = self%assert_elemental(val, ref) .eqv. .true.

    if(self%status) self%pass_count = self%pass_count + 1

    call print_test_result(self%name, self%status)
  end subroutine assert_scalar
  
  subroutine assert_array(self, val, ref)
    class(testify), intent(inout) :: self
    class(*), intent(in) :: val(:), ref(:)
    
    self%test_count = self%test_count + 1
    
    self%status = all(self%assert_elemental(val, ref) .eqv. .true.)

    if(self%status) self%pass_count = self%pass_count + 1

    call print_test_result(self%name, self%status)
  end subroutine assert_array

  subroutine print_test_result(name, passed)
    logical, intent(in) :: passed
    character(*), intent(in) :: name
    
    character(:), allocatable :: message_head, message_butt

    message_head = 'Test name: ' // name // ' => ' // achar(27)
    message_butt = achar(27) // '[0m'
    if(passed) then
       print *, message_head // '[32m PASSED! :)' // message_butt
    else
       print *, message_head // '[31m FAILED! :(' // message_butt
    end if
  end subroutine print_test_result
  
  elemental logical function assert_elemental(self, val, ref)
    class(testify), intent(in) :: self
    class(*), intent(in) :: val, ref

    select type(val)
    type is(logical)
       select type(ref)
       type is(logical)
          assert_elemental = val .eqv. ref
       end select

    type is(integer(i8))
       select type(ref)
       type is(integer(i8))
          assert_elemental = val == ref
       end select

    type is(integer(i16))
       select type(ref)
       type is(integer(i16))
          assert_elemental = val == ref
       end select

    type is(integer(i32))
       select type(ref)
       type is(integer(i32))
          assert_elemental = val == ref
       end select

    type is(integer(i64))
       select type(ref)
       type is(integer(i64))
          assert_elemental = val == ref
       end select

    type is(real(r32))
       select type(ref)
       type is(real(r32))
          assert_elemental = val == ref
       end select
       
    type is(real(r64))
       select type(ref)
       type is(real(r64))
          assert_elemental = val == ref
       end select

    type is(real(r128))
       select type(ref)
       type is(real(r128))
          assert_elemental = val == ref
       end select
    end select
  end function assert_elemental
  
  subroutine report(self)
    class(testify), intent(in) :: self

    print*, 'Total number of tests: ', self%test_count
    print*, 'Number of tests passed: ', self%pass_count
    print*, 'Number of tests failed: ', self%test_count - self%pass_count
  end subroutine report
end module testify_m
