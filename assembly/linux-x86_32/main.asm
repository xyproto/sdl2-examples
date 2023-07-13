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
	push 62001
	call SDL_Init
	add esp, 4
	cmp eax, 0
	jl init_err
	push 4
	push 387
	push 620
	push 100
	push 100
	push create_window_arg0
	call SDL_CreateWindow
	add esp, 24
	cmp eax, 0
	je create_window_err
	mov ebx, eax
	push 6
	push -1
	push eax
	call SDL_CreateRenderer
	add esp, 12
	cmp eax, 0
	je create_renderer_err
	mov esi, eax
	push readbin
	push bmp_path
	call SDL_RWFromFile
	add esp, 8
	push 1
	push eax
	call SDL_LoadBMP_RW
	add esp, 8
	cmp eax, 0
	je load_bmp_err
	mov edi, eax
	push eax
	push esi
	call SDL_CreateTextureFromSurface
	add esp, 8
	cmp eax, 0
	je create_tfs_err
	push edi
	mov edi, eax
	call SDL_FreeSurface
	add esp, 4
	push ebx
	xor ebx, ebx
loop0:
	push esi
	call SDL_RenderClear
	add esp, 4
	push 0
	push 0
	push edi
	push esi
	call SDL_RenderCopy
	add esp, 16
	push esi
	call SDL_RenderPresent
	add esp, 4
	push 100
	call SDL_Delay
	add esp, 4
	inc ebx
	cmp ebx, 20
	jl loop0
	pop ebx
	push edi
	call SDL_DestroyTexture
	add esp, 4
	push esi
	call SDL_DestroyRenderer
	add esp, 4
	push ebx
	call SDL_DestroyWindow
	add esp, 4
	call SDL_Quit
	push 0
	call exit
init_err:
	call SDL_GetError
	push eax
	push init_err_msg
	call printf
	add esp, 8
	push 8
	call exit
create_window_err:
	call SDL_GetError
	push eax
	push create_window_err_msg
	call printf
	add esp, 8
	push 8
	call exit
create_renderer_err:
	call SDL_GetError
	push eax
	push create_renderer_err_msg
	call printf
	add esp, 8
	push ebx
	call SDL_DestroyWindow
	add esp, 4
	call SDL_Quit
	push 8
	call exit
load_bmp_err:
	call SDL_GetError
	push eax
	push load_bmp_err_msg
	call printf
	add esp, 8
	push esi
	call SDL_DestroyRenderer
	add esp, 4
	push ebx
	call SDL_DestroyWindow
	add esp, 4
	call SDL_Quit
	push 8
	call exit
create_tfs_err:
	call SDL_GetError
	push eax
	push create_tfs_err_msg
	call printf
	add esp, 8
	push edi
	call SDL_FreeSurface
	add esp, 4
	push esi
	call SDL_DestroyRenderer
	add esp, 4
	push ebx
	call SDL_DestroyWindow
	add esp, 4
	call SDL_Quit
	push 8
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
