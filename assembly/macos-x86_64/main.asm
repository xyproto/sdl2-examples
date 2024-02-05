section .text
global _main

extern _printf
extern _exit
extern _SDL_Init
extern _SDL_CreateWindow
extern _SDL_CreateRenderer
extern _SDL_LoadBMP_RW
extern _SDL_RWFromFile
extern _SDL_CreateTextureFromSurface
extern _SDL_FreeSurface
extern _SDL_RenderClear
extern _SDL_RenderCopy
extern _SDL_RenderPresent
extern _SDL_Delay	
extern _SDL_DestroyTexture
extern _SDL_DestroyRenderer
extern _SDL_DestroyWindow
extern _SDL_Quit
extern _SDL_GetError

_main:
	sub rsp, 8
	mov rdi, 62001
	call _SDL_Init
	cmp rax, 0
	jl init_err
	mov rdi, create_window_arg0
	mov rsi, 100
	mov rdx, 100
	mov rcx, 960
	mov r8, 720
	mov r9, 4
	call _SDL_CreateWindow
	cmp rax, 0
	je create_window_err
	mov r12, rax
	mov rdi, rax
	mov rsi, -1
	mov rdx, 6
	call _SDL_CreateRenderer
	cmp rax, 0
	je create_renderer_err
	mov r13, rax
	mov rdi, bmp_path
	mov rsi, readbin
	call _SDL_RWFromFile
	mov rdi, rax
	mov rsi, 1
	call _SDL_LoadBMP_RW
	cmp rax, 0
	je load_bmp_err
	mov r14, rax
	mov rdi, r13
	mov rsi, rax
	call _SDL_CreateTextureFromSurface
	cmp rax, 0
	je create_tfs_err
	mov r15, rax
	mov rdi, r14
	call _SDL_FreeSurface
	xor r14, r14
loop0:
	mov rdi, r13
	call _SDL_RenderClear
	mov rdi, r13
	mov rsi, r15
	mov rdx, 0
	mov rcx, 0
	call _SDL_RenderCopy	
	mov rdi, r13
	call _SDL_RenderPresent	
	mov rdi, 100
	call _SDL_Delay
	inc r14
	cmp r14, 20
	jl loop0
	mov rdi, r15
	call _SDL_DestroyTexture
	mov rdi, r13
	call _SDL_DestroyRenderer
	mov rdi, r12
	call _SDL_DestroyWindow
	call _SDL_Quit
	mov rdi, 0
	call _exit
init_err:
	call _SDL_GetError
	mov rsi, rax
	mov rdi, init_err_msg
	call _printf
	mov rdi, 8
	call _exit
create_window_err:
	call _SDL_GetError
	mov rsi, rax
	mov rdi, create_window_err_msg
	call _printf
	mov rdi, 8
	call _exit
create_renderer_err:
	call _SDL_GetError
	mov rsi, rax
	mov rdi, create_renderer_err_msg
	call _printf
	mov rdi, r12
	call _SDL_DestroyWindow
	call _SDL_Quit
	mov rdi, 8
	call _exit
load_bmp_err:
	call _SDL_GetError
	mov rsi, rax
	mov rdi, load_bmp_err_msg
	call _printf
	mov rdi, r13
	call _SDL_DestroyRenderer
	mov rdi, r12
	call _SDL_DestroyWindow
	call _SDL_Quit
	mov rdi, 8
	call _exit
create_tfs_err:
	call _SDL_GetError
	mov rsi, rax
	mov rdi, create_tfs_err_msg
	call _printf
	mov rdi, r14
	call _SDL_FreeSurface
	mov rdi, r13
	call _SDL_DestroyRenderer
	mov rdi, r12
	call _SDL_DestroyWindow
	call _SDL_Quit
	mov rdi, 8
	call _exit


section .data

init_err_msg            db "SDL_Init Error: %s", 10, 0

create_window_arg0      db "Hello World!", 0
	
create_window_err_msg   db "SDL_CreateWindow Error: %s", 10, 0

create_renderer_err_msg db "SDL_CreateRenderer Error: %s", 10, 0

bmp_path                db "../../img/grumpy-cat.bmp", 0

readbin                 db "rb", 0

load_bmp_err_msg        db "SDL_LoadBMP Error: %s", 10, 0

create_tfs_err_msg      db "SDL_CreateTextureFromSurface Error: %s", 10, 0
