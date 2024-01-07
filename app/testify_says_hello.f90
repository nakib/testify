program testify_says_hello
  use testify_m, only : testify

  implicit none

  integer :: itest
  type(testify) :: test1, test2, test3, test_array, all_tests

  call create_new_chat
  
  call chat("Hello from testify!")
  call chat("Here's what you can do with it:")
  
  call chat("Create a test with a meaningful name as follows:")
  call chat(" test1 = testify('2 + 3 = 5')", form = 'c')
  call chat("Assert as follows:")
  call chat(" call test1%assert(val = 2 + 3, ref = 5)", form = 'c')
  call chat("This creates the following output:")
  
  test1 = testify('2 + 3 = 5')
  call test1%assert(val = 2 + 3, ref = 5)

  call chat("2 + 3 = 5 =>  PASSED! :)", form = 'v', quiet = 1)

  call chat("You can get the status of this test with:")
  
  call chat(" test1%get_status()", form = 'c')

  
  call chat("This will return a logical which, in this case, is true.")
  
  call chat("You can pass in a tolerance for real arithmetic:")
  
  call chat(" test2 = testify('1.0 - 9.0e-1 = 0.1 with tolerance 1.0e-7')", form = 'c')
  call chat(" call test2%assert(val = 1.0 - 9.0e-1, ref = 0.1, tol = 1.0e-7)", form = 'c')
  
  call chat("This creates the following output:")
  
  test2 = testify('1.0 - 0.9 = 0.1 with tolerance 1.0e-7')
  call test2%assert(val = 1.0 - 0.9, ref = 0.1, tol = 1.0e-7)

  call chat("1.0 - 0.9 = 0.1 with tolerance 1.0e-7 =>  PASSED! :)", &
       form = 'v', quiet = 1)
  
  call chat("Your turn. Define test3 to do the same as test2 but with a tolerance 1.0e-8.")
  call chat("You should get the following output:")
  
  test3 = testify("1.0 - 0.9 = 0.1 with tolerance 1.0e-8")
  call test3%assert(val = 1.0 - 0.9, ref = 0.1, tol = 1.0e-8)

  call chat("1.0 - 0.9 = 0.1 with tolerance 1.0e-8 =>  FAILED! :(", &
       form = 'v', quiet = 1)

  call chat("Instead of an unspecified precision, you can pass in any of the iso_fortran_env precisions.")
  call chat("However, 'assert' will not accept 'val', 'ref', and 'tol' with different precisions.")

  
  call chat("You can combine individual tests with a '+' operator as follows:")
  
  call chat(" all_tests = test1 + test2 + test3", form = 'c')
  
  call chat("Following this you can create a combined report by saying:")
  
  call chat(" call all_tests%report", form = 'c')
  
  call chat("This prints to screen a summary:")
  
  all_tests = test1 + test2 + test3
  call all_tests%report

  call chat("------>", &
       form = 'v', quiet = 1)
  call chat("Total number of tests:        3", &
       form = 'v', quiet = 1)
  call chat("Number of tests passed:       2", &
       form = 'v', quiet = 1)
  call chat("Number of tests failed:       1", &
       form = 'v', quiet = 1)
  call chat("<------", &
       form = 'v', quiet = 1)
  
contains

  subroutine create_new_chat
    !! This creates and initiates a new chat.org file.
    
    integer :: io
    logical :: file_exists
    character(8) :: orgfile = "chat.org"

    !Does chat.org?
    inquire(file = orgfile, exist = file_exists)
    
    if(.not. file_exists) then
       open(newunit = io, file = orgfile, status = "new", &
            position = "append", action = "write")
    else
       open(newunit = io, file = orgfile, status = "replace", &
            action = "write")
    end if
    write(io, '(A)') "* Testify"
    close(io)
  end subroutine create_new_chat
  
  subroutine chat(string, form, quiet)
    !! This writes sentences/code snippets
    !! out to the terminal and also to an org
    !! file which can then be used as the github README.
    
    character(*), intent(in) :: string
    character, optional, intent(in) :: form
    integer, optional, intent(in) :: quiet

    character(8) :: orgfile = "chat.org"
    logical :: file_exists
    integer :: io
    
    !Write to terminal
    if(.not. present(quiet)) write(*, '(A)') string
    
    !Does chat_output exist?
    inquire(file = orgfile, exist = file_exists)

    if(file_exists) then
       open(newunit = io, file = orgfile, status = "old", &
            position = "append", action = "write")

       if(present(form)) then
          select case(form)
          case('c') !code
             write(io, '(A)') "#+BEGIN_SRC fortran"
             write(io, '(A)') "    " // string
             write(io, '(A)') "#+END_SRC"
          case('v') !verbatim
             write(io, '(A)')
             write(io, '(A)') "    ~" // string // "~"
             write(io, '(A)')
          case default
             write(io, '(A)')
             write(io, '(A)') "    " // string
             write(io, '(A)')
          end select
       else
          write(io, '(A)') "    " // string
       end if
              
       close(io)
    else
       error stop
    end if
  end subroutine chat
end program testify_says_hello
