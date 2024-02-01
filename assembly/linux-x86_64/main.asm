section .text
global _start

extern printf
extern exit
extern SDL_Init
extern SDL_CreateWindow
extern SDL_CreateRenderer
extern SDL_LoadBMP_RW
extern SDL_RWFromFile
extern SDL_CreateTextureFromSurface
extern SDL_FreeSurface
extern SDL_RenderClear
extern SDL_RenderCopy
extern SDL_RenderPresent
extern SDL_Delay
extern SDL_DestroyTexture
extern SDL_DestroyRenderer
extern SDL_DestroyWindow
extern SDL_Quit
extern SDL_GetError

_start:
  mov rdi, 62001
  call SDL_Init
  cmp rax, 0
  jl init_err
  mov rdi, create_window_arg0
  mov rsi, 100
  mov rdx, 100
  mov rcx, 620
  mov r8, 387
  mov r9, 4
  call SDL_CreateWindow
  cmp rax, 0
  je create_window_err
  mov r12, rax
  mov rdi, rax
  mov rsi, -1
  mov rdx, 6
  call SDL_CreateRenderer
  cmp rax, 0
  je create_renderer_err
  mov r13, rax
  mov rdi, bmp_path
  mov rsi, readbin
  call SDL_RWFromFile
  mov rdi, rax
  mov rsi, 1
  call SDL_LoadBMP_RW
  cmp rax, 0
  je load_bmp_err
  mov r14, rax
  mov rdi, r13
  mov rsi, rax
  call SDL_CreateTextureFromSurface
  cmp rax, 0
  je create_tfs_err
  mov r15, rax
  mov rdi, r14
  call SDL_FreeSurface
  xor r14, r14
loop0:
  mov rdi, r13
  call SDL_RenderClear
  mov rdi, r13
  mov rsi, r15
  mov rdx, 0
  mov rcx, 0
  call SDL_RenderCopy
  mov rdi, r13
  call SDL_RenderPresent
  mov rdi, 100
  call SDL_Delay
  inc r14
  cmp r14, 20
  jl loop0
  mov rdi, r15
  call SDL_DestroyTexture
  mov rdi, r13
  call SDL_DestroyRenderer
  mov rdi, r12
  call SDL_DestroyWindow
  call SDL_Quit
  mov rdi, 0
  call exit
init_err:
  call SDL_GetError
  mov rsi, rax
  mov rdi, init_err_msg
  call printf
  mov rdi, 8
  call exit
create_window_err:
  call SDL_GetError
  mov rsi, rax
  mov rdi, create_window_err_msg
  call printf
  mov rdi, 8
  call exit
create_renderer_err:
  call SDL_GetError
  mov rsi, rax
  mov rdi, create_renderer_err_msg
  call printf
  mov rdi, r12
  call SDL_DestroyWindow
  call SDL_Quit
  mov rdi, 8
  call exit
load_bmp_err:
  call SDL_GetError
  mov rsi, rax
  mov rdi, load_bmp_err_msg
  call printf
  mov rdi, r13
  call SDL_DestroyRenderer
  mov rdi, r12
  call SDL_DestroyWindow
  call SDL_Quit
  mov rdi, 8
  call exit
create_tfs_err:
  call SDL_GetError
  mov rsi, rax
  mov rdi, create_tfs_err_msg
  call printf
  mov rdi, r14
  call SDL_FreeSurface
  mov rdi, r13
  call SDL_DestroyRenderer
  mov rdi, r12
  call SDL_DestroyWindow
  call SDL_Quit
  mov rdi, 8
  call exit


section .data

init_err_msg            db "SDL_Init Error: %s", 10, 0

create_window_arg0      db "Hello World!", 0

create_window_err_msg   db "SDL_CreateWindow Error: %s", 10, 0

create_renderer_err_msg db "SDL_CreateRenderer Error: %s", 10, 0

bmp_path                db "../../img/grumpy-cat.bmp", 0

readbin                 db "rb", 0

load_bmp_err_msg        db "SDL_LoadBMP Error: %s", 10, 0

create_tfs_err_msg      db "SDL_CreateTextureFromSurface Error: %s", 10, 0

;asdf
