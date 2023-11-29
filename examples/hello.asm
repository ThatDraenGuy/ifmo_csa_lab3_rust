hello: word "Hello, world!"

start: 
    mov eax, hello
    call print_string
    exit

print_string:     ;EAX - указатель на начало строки (размер)
    mov edx, eax    ;используем edx в  качестве указателя на текущий набор символов
    mov ecx, [eax]  ;читаем длину строки в ecx
    jz print_string_end ;если длина нулевая - завершаем работу

print_string_main_loop:
    inc edx         ;переходим к следующей ячейке с символами
    mov eax, [edx]  ;читаем набор символов в eax
    mov ebx, 4      ;используем ebx как счётчик символов в одной ячейке

print_string_word_loop:
    out 1           ;выводим символ

    dec ecx         ;уменьшаем оставшуюся длину строки. Если строка кончилась - завершаем работу
    jz print_string_end

    dec ebx         ;уменьшаем число оставшихся символов в текущей ячейке. Если кончились - переходим к следующей
    jz print_string_main_loop

    shr eax, 8      ;сдвигаем eax для получения следующего символа в младшем байте
    jmp print_string_word_loop

print_string_end:
    ret