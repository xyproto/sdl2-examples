section .text
global Start

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

Start:
	sub rsp, 56
	mov rcx, 62001
	call SDL_Init
	cmp rax, 0
	jl init_err
	mov rcx, create_window_arg0
	mov rdx, 100
	mov r8, 100
	mov r9, 960
	mov qword [rsp+32], 720
	mov qword [rsp+40], 4
	call SDL_CreateWindow
	cmp rax, 0
	je create_window_err
	mov r12, rax
	mov rcx, rax
	mov rdx, -1
	mov r8, 6
	call SDL_CreateRenderer
	cmp rax, 0
	je create_renderer_err
	mov r13, rax
	mov rcx, bmp_path
	mov rdx, readbin
	call SDL_RWFromFile
	mov rcx, rax
	mov rdx, 1
	call SDL_LoadBMP_RW
	cmp rax, 0
	je load_bmp_err
	mov r14, rax
	mov rcx, r13
	mov rdx, rax
	call SDL_CreateTextureFromSurface
	cmp rax, 0
	je create_tfs_err
	mov r15, rax
	mov rcx, r14
	call SDL_FreeSurface
	xor r14, r14
loop0:
	mov rcx, r13
	call SDL_RenderClear
	mov rcx, r13
	mov rdx, r15
	mov r8, 0
	mov r9, 0
	call SDL_RenderCopy	
	mov rcx, r13
	call SDL_RenderPresent	
	mov rcx, 100
	call SDL_Delay
	inc r14
	cmp r14, 20
	jl loop0
	mov rcx, r15
	call SDL_DestroyTexture
	mov rcx, r13
	call SDL_DestroyRenderer
	mov rcx, r12
	call SDL_DestroyWindow
	call SDL_Quit
	add rsp, 56
	mov rcx, 0
	call exit
init_err:
	call SDL_GetError
	mov rdx, rax
	mov rcx, init_err_msg
	call printf
	add rsp, 56
	mov rcx, 8
	call exit
create_window_err:
	call SDL_GetError
	mov rdx, rax
	mov rcx, create_window_err_msg
	call printf
	add rsp, 56
	mov rcx, 8
	call exit
create_renderer_err:
	call SDL_GetError
	mov rdx, rax
	mov rcx, create_renderer_err_msg
	call printf
	mov rcx, r12
	call SDL_DestroyWindow
	call SDL_Quit
	add rsp, 56
	mov rcx, 8
	call exit
load_bmp_err:
	call SDL_GetError
	mov rdx, rax
	mov rcx, load_bmp_err_msg
	call printf
	mov rcx, r13
	call SDL_DestroyRenderer
	mov rcx, r12
	call SDL_DestroyWindow
	call SDL_Quit
	add rsp, 56
	mov rcx, 8
	call exit
create_tfs_err:
	call SDL_GetError
	mov rdx, rax
	mov rcx, create_tfs_err_msg
	call printf
	mov rcx, r14
	call SDL_FreeSurface
	mov rcx, r13
	call SDL_DestroyRenderer
	mov rcx, r12
	call SDL_DestroyWindow
	call SDL_Quit
	add rsp, 56
	mov rcx, 8
	call exit


section .data

init_err_msg            db "SDL_Init Error: %s", 10, 0

create_window_arg0      db "Hello World!", 0
	
create_window_err_msg   db "SDL_CreateWindow Error: %s", 10, 0

create_renderer_err_msg db "SDL_CreateRenderer Error: %s", 10, 0

bmp_path                db "..\..\img\grumpy-cat.bmp", 0

readbin                 db "rb", 0

load_bmp_err_msg        db "SDL_LoadBMP Error: %s", 10, 0

create_tfs_err_msg      db "SDL_CreateTextureFromSurface Error: %s", 10, 0	
