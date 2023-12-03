# Лабораторная работа №3 по "Архитектуре компьютера" - 🦀 Rust edition 🦀
**Хайкин Олег Игоревич, P33312**  
Вариант - asm | cisc | neum | hw | tick | struct | stream | port | pstr | prob2 | \[4\]char
## Разбор структуры репозитория
Детали о структуре репозитория можно прочитать [здесь](./template.md)

## Язык программирования
```
program ::= terms

terms ::= term
        | terms term

term ::= instruction
       | label_def
       | directive


instruction ::= math_op
              | branch_op
              | alter_op
              | io_op
              | control_op
              | stack_op

math_op ::= math_opcode math_args
math_opcode ::= 'mov'
              | 'add'
              | 'sub'
              | 'cmp'
              | 'shl'
              | 'shr'

math_args ::= reg_id ',' reg_id
            | mem_by_reg_id ',' reg_id
            | reg_id ',' mem_by_reg_id
            | reg_id ',' label_ref
            | label_ref ',' reg_id
            | reg_id ',' immed


branch_op ::= branch_opcode branch_args
branch_opcode ::= 'jmp'
                | 'jz'
                | 'jnz'
                | 'js'
                | 'jns'
branch_args ::= label_ref


alter_op ::= alter_opcode alter_args
alter_opcode ::= 'inc'
               | 'dec'
alter_args ::= reg_id


io_op ::= io_opcode io_args
io_opcode ::= 'in'
            | 'out'
io_args ::= port_id


control_op ::= 'exit'
             | 'div'


stack_op ::= 'ret'
           | 'call' label_ref
           | 'pop' reg_id
           | 'push' push_args
push_args ::= reg_id
            | immed


mem_by_reg_id ::= '[' reg_id ']'
reg_id ::= 'eax'
         | 'ebx'
         | 'ecx'
         | 'edx'
         | 'esp'
         | 'eip'

immed ::= u32
        | i32
        | symbol
port_id ::= u16

directive ::= "word" literal
literal ::= immed
          | string

label_ref ::= label_id
label_def ::= label_id ':'
label_id ::= <combination of any english lowercase letter, number or undeerscore>


u32 ::= <число u32>
i32 ::= <число i32>
symbol ::= <символ, заключённый в одинарные кавычки>
string ::= <строка, заключённая в двойные кавычки. Поддерживает эскейпинг через обратный слэш внутри строки>
```

Способы выделения памяти:
- Статически, через директиву word
- Локально на стеке (технически, это можно назвать выделением вручную через ручной вызов инструкций push/pop или смещение стэк-поинтера)

Переменные не существуют.

В качестве литералов (immed-значений) поддерживаются u32/i32 числа и одиночные символы (char). Для директивы word также поддерживаются строковые литералы.


## Организация памяти
Память процессора представлена массивом из машинных слов. Каждое машинное слово занимает 4 байта. Система рассчитана на 32-битные адреса, поддерживая до U32_MAX ячеек в памяти процессора.

Стоит отметить, что такая память достигала бы 16 гигабайт. В целях работы с симуляцией реальный размер памяти был уменьшен до 4096 ячеек. Адресация при этом не изменена и способна поддерживать "полную" память.

Отображение данных и инструкций в память примитивна - все данные и инструкции располагаются последовательно, начиная с начальной ячейки памяти, в порядке, в котором они были заданы с исходном asm-коде.

Программисту доступны:
- 5 регистров (Accumulator, Base, Count, Data, Stack Pointer и Instruction Pointer)
- Память процессора (путём работы с метками, реальными адресами или указателем на стек)

Статические данные располагаются в памяти процессора. Динамические данные располагабтся на стэке. Куча не поддерживается.


## Система команд
Машинное слово занимает 4 байта. Тип данных определяется только интерпретацией программиста - с точки зрения процессора всё является просто набором бит.

Регистры, как и машинные слова, рассчитаны на 4 байта (32 бита). 

Реализована только прямая адресация.

Устройства ввода-вывода подключены к портам процессора. Доступ к ним осуществуляется через id порта. Система поддерживает до 2**16 различных портов.

Прерывания отсутствуют. Поток управления управляется с помощью eip (регистра-указателя на следующую инструкцию).

Инструкции занимают от одного до двух машинных слов. Необходимость в использовании двух машинных слов для одной инструкции появляется из-за размера immed-значений и адресов (оба могут достигать 4-ёх байт, т.е. целого машинного слова, из-за чего места под код операции и тип её аргументов не остаётся)

Инструкции условно делятся на несколько категорий:

### MathOp
"Математические" операции. Эти инструкции принимают 2 аргумента - dest и src. Они выполняют некую операцию над 2-мя операндами (взятыми из dest и src соответственно) и помещают значение в dest. Операндами могут быть:
- пара регистров
- dest - регистр, src - значение в памяти по адресу из регистра
- dest - значение в памяти по адресу из регистра, src - регистр
- dest - регистр, src - значение в памяти по адресу-константе
- dest - значение в памяти по адресу-константе, src - регистр
- dest - регистр, src - immed-значение (литерал)

В этой категории реализованы следующие операции:
- MOV - записывает значение src в dest
- ADD - прибавляет значение src к dest
- SUB - вычитает значение src из dest
- CMP - вычитает значение src из dest, но не изменяет dest, а только выставляет флаги
- SHL - арифметический сдвиг src на dest бит влево
- SHR - арифметический сдвиг src на dest бит вправо

### BranchOp
Операции ветвления. Изменяют значение eip при выполнении какого-то условия. Их аргументом является адрес, куда совершается переход при выполнении условия (представляется меткой).

Условия представлены проверкой значения какого-то из флагов процессора. Реализованы переходы на основе следующих флагов:
- ZF (zero flag) - флаг, отображающий нулевое значение
- SF (sign flag) - флаг, отображающий "знак" значения (наличие единицы в его старшем бите)

В этой категории реализованы следующие операции:
- JMP - безусловный переход
- JZ - переход, если ZF=1
- JNZ - переход, если ZF=0
- JS - переход, если SF=1
- JNS - переход, если SF=0

### AlterOp
Операции альтерации. Изменяют значение регистра, переданного им как аргумент.

В этой категории реализованы следующие операции:
- INC - инкремент регистра
- DEC - декремент регистра

### IoOp
Операции ввода-вывода. Принимают в качестве аргумента id порта, к которму подключено устройство. Выполняют ввод/вывод между выбранным устройством и младшим байтом аккумулятора.

В этой категории реализованы следующие операции:
- IN - чтение байта из устройства в младший байт аккумулятора
- OUT - запись байта из младшего байта аккумулятора в устройство

### ControlOp
Операции управления. Не принимают аргументов и выполняют какую-то "особую" операцию.

В этой категории реализованы следующие операции:
- EXIT - выполняет завершение работы процессора
- DIV - выполняет деление значения в аккумуляторе на значение из base-регистра. Помещает частное в аккумулятор и остаток в дата-регистр.

### StackOp
Операции работы со стеком. Имеют разный набор аргументов и объединены в категорию только по смыслу.

Стек реализован на основе esp-регистра (указателя на вершину стека). При инициализации стек указывает на адрес последней ячейки памяти + 1. Стек "растёт" вниз, путём декремента esp-регистра. При помещении значения в стек esp-регистр декрементируется и значение кладётся в память по адресу из него. При удалении значения из стека, выполняется обращение к памяти по адресу из esp-регистра и его полседующий инкремент.

В этой категории реализованы следующие операции:
- PUSH - кладёт значение в стек, аргументом является регистр или immed-значение.
- POP - достаёт значения из стека и кладёт его в регистр, переданный как аргумент
- CALL - вызов процедуры(функции). Помещает в стек текущее значение eip-регистра и заменяет его адресом, переданным как аргумент, тем самым осуществляя переход в тело процедуры
- RET - выполняет возвращение из процедуры. Достаёт значение из стека и кладёт его в eip. Не принимает аргументов. Эта операция аналогична вызову `pop eip`

## Транслятор
**Входные данные**:
- Путь к файлу с исходным кодом
- Путь к файлу для размещения результата

**Выходные данные**:
- Транслированная программа, размещённая в указанном файле

Вызов транслятора осуществляется следующим образом:  
`./translator <source_file> <target_file>`

Транслятор работает в 2 прохода:

В первом проходе транслятор рассчитывает адреса всех указанных меток.

Во втором проходе транслятор оссуществляет реальный перевод из языка в машинные инструкции, подставляя вместо обращений к меткам их рассчитанные адреса.


## Модель процессора
**Входные данные**:
- Путь к файлу с программой
- Путь к файлу с данными, которые вводятся в процессор (через устройство в порте 0)

**Выходные данные**:
- Вывод данных из процессора (через устройство в порте 1)
- Данные логирования состояния регистров и флагов процессора

Вызов виртуальной мащшины осуществляется следующим образом:  
`./machine <code_file> <input_file>`

### Схема Datapath
```
                    to Decoder
                  ▲     ▲     ▲                       │from Decoder               │from Decoder    ▲to Decoder
                  │     │     │                       │                           │                │
                  │     │     │                       │                           │                │
                  │     │     │                       │                           │                │
                  │     │     │                       │                           │                │
                  │     │     │                       │                           │                │
                  │     │     │                       │ ┌────────────────┐        │                │
                  │     │     │          ┌────────────┼─┤     Memory     │    ┌───▼────┐           │
                  │     │     │          │            │ │                ◄────┤Mem Addr◄──┐        │
                  │     │     │   sel ┌──▼──┐         │ │                │    └────────┘  │        │
                  │     │     │   ────►DEMUX│         │ │                │                │        │
                  │     │     │       └┬─┬─┬┘         │ │                │RD SIG          │        │
                  │     │     │        │ │ │          │ │                ◄────            │        │
                  │     │     └────────┘ │ │          │ │                │                │        │
                  │     │                │ │          │ │                │                │        │
                  │     │     ┌──────────┘ │          │ │                │WR SIG          │        │
                  │     │     │            │          │ │                ◄───             │        │
                  │  ┌──┴─────▼────┐       │          │ │                │                │        │
to/from Ports     │  │  Registers  │       │          │ │                │   ┌──────────┐ │        │
◄─────────────────┼──►             │   ┌───▼───────┐  │ │                ◄───┤Mem In Buf│ │        │
                  │  │             │   │Mem Out Buf│  │ │                │   └─▲────────┘ │        │
                  │  └─▲─────┬─────┘   └────┬──────┘  │ └────────────────┘     │          │        │
                  │    │     │              │         │                        │          │        │
                  │    │     │ ┌────────────┤         │                        │          │        │
                  │    │     │ │            │         │                        │          │        │
                  │    │     ├─┼──────────┐ │ ┌───────┘                        │          │        │
                  │    │     │ │          │ │ │                                │          │        │
                  │    │ sel┌▼─▼┐        ┌▼─▼─▼┐sel                            │          │        │
                  │    │ ───►MUX│        │ MUX ◄───                            │          │        │
                  │    │    └─┬─┘        └─┬───┘                               │          │        │
                  │    │      │            │                                   │          │        │
                  │    │    ┌─▼────────────▼─┐                                 │          │        │
                  │    │    │      ALU       │                                 │          │        │
                  │    │    │                │ALU signals                      │          │        │
                  │    │    │                ◄───────                          │          │        │
                  │    │    │                │                                 │          │        │
                  │    │    │                │                                 │          │        │
                  │    │    └───────┬────────┘                                 │          │        │
                  │    │            │                        ┌─────┐           │          │        │
                  │    │            ├────────────────────────►flags├───────────┼──────────┼────────┘
                  │    │            │                        └─────┘           │          │
                  │    │       ┌────▼────┐sel                                  │          │
                  │    │       │  DEMUX  ◄───                                  │          │
                  │    │       └─┬─┬──┬─┬┘                                     │          │
                  │    │         │ │  │ │                                      │          │
                  │    └─────────┘ │  │ └──────────────────────────────────────┘          │
                  │                │  │                                                   │
                  └────────────────┘  └───────────────────────────────────────────────────┘
```

### Схема ControlUnit
```
                                                                         ┌───────────┐
                                                                         │ microcode │
                                                                         │ decoder   │
                                                                         └───┬──────┬┘
                                                                             │      │
                                                                             │      │
                                                                             │      │
                                                                             │      │
┌───────────────────────────────────────────────────────────────────┐        │      │
│                            Decoder                                │        │      │
│                                                                   │        │      │
│                                                                   │        │      │
│                                                                   │        │      │
│                                                                   │Signals │      │
│                        *чёрная магия*                             ◄────────┘      │
│                                                                   │               │
│                                                                   │               │
│                           operand pipe                            │               │
│                        ┌─────────────────┐                        │               │
│                        │                 │                        │               │
│        ┌───────────────┴──┐            ┌─▼────────────┐           │               │
│        │Instruction buffer│            │Operand buffer│           │               │
│        └─▲────────────────┘            └─▲──────────┬─┘           │               │
│          │                               │          │             │               │
│          │                               │          │             │               │
│          │                               │          │             ├───────────────┤
│          │                               │          │             │               │
│          │  ┌────────────────────────────┘          │             │               │
│          │  │                                       │             │               │
│         ┌┴──┴─┐                                     │             │               │
│         │DEMUX│                                     │             │ flags         │
│         └─▲───┘                                     │             ◄─────────┐     │
│           │                                         │             │         │     │
└───────────┼─────────────────────────────────────────┼─────────────┘         │     │
            │                                         │                       │     │
            │                                         │                       │     │
            │                                         │                       │     │
            │                                         │                       │     │
            │                                         │                       │     │
            │                                         │                       │     │
            │from ALU DEMUX                           ▼to right ALU MUX       │     │
┌───────────┴───────────────────────────────────────────────────────┐         │     │
│                              Datapath                             │         │     │
│                                                                   ├─────────┘     │
│                                                                   │               │
│                                                                   │               │
│                                                                   ◄───────────────┘
│                                                                   │ Signals
│                                                                   │
│                                                                   │
└───────────────────────────────────────────────────────────────────┘
```



## Тестирование
### Интеграционное тестирование
Для интеграционного тестирования используется библиотека [insta](https://crates.io/crates/insta), предоставляющий возможность тестирования на основе снапшотов.  
Я считаю эту библиотеку наилучшим аналогом golden-тестов в рамках Rust'а. Технически, снапшоты хранят в себе только результаты, а не входные данные, но это исправляется просто ручной обработкой файлов входных данных, настроенной мною.

В директории [inputs](./tests/inputs/) размещаются входные данные для отдельных тестов. Каждый yml файл в этой директории содержит ассемблер-код для транслятора и входные данные для виртуальной машины. Интеграционный тест совершает цикл обработки отдельно для каждого из этих файлов.

Для каждого файла создаётся набор временных файлов для входных данных транслятора и машины и файла с машинным кодом. Выводы в stdout (вывод виртаульной машины) и stderr (данные логирования) также захватываются. По окончанию работы транслятора и машины (не важно, успешной или нет), создаётся специальный снапшот-файл.

В зависимости от настройки фреймоворка для тестирования, новые снапшот-файлы могут:
- мгновенно заменить старые
- отправиться на review diff'а программистом
- удалиться, если они совпадают со старыми (нет изменений)
- "провалить" тест, если существует разница со старыми снапшот-файлами
- и т.д. (лучше про это описывается в рамках документации самого фреймворка)

### CI
[Workflow](./.github/workflows/rust.yml) настроен для Github Actions, и производит следующиие действия:
- Проверка форматирования
- Проверка линтером
- Тестирование
- Сборка

CI настроен для работы при выполнении push и pull request на ветках master и workflow

### Тестовые ассемблер-программы
#### hello
[input-file](./tests/inputs/hello.yml)

[snapshot-file](./tests/snapshots/integration__test@hello.yml.snap)

Исходный код:
```asm
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
```

Машинный код:
```ron
  (
      code: [
          (
              debug_info: (
                  src_info: (
                      src_line: 1,
                      src_symb: 27,
                  ),
                  addr: (1),
              ),
              word: Data((13)),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 1,
                      src_symb: 27,
                  ),
                  addr: (2),
              ),
              word: Data((1819043144)),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 1,
                      src_symb: 27,
                  ),
                  addr: (3),
              ),
              word: Data((1998597231)),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 1,
                      src_symb: 27,
                  ),
                  addr: (4),
              ),
              word: Data((1684828783)),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 1,
                      src_symb: 27,
                  ),
                  addr: (5),
              ),
              word: Data((33)),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 4,
                      src_symb: 7,
                  ),
                  addr: (6),
              ),
              word: OpHigher(Math((
                  opcode: Mov,
                  args: RegImmed(Accumulator, 0),
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 4,
                      src_symb: 7,
                  ),
                  addr: (7),
              ),
              word: OpLower(Math(RegImmed(1))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 5,
                      src_symb: 8,
                  ),
                  addr: (8),
              ),
              word: OpHigher(Stack((
                  opcode: Call,
                  args: Immed(0),
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 5,
                      src_symb: 8,
                  ),
                  addr: (9),
              ),
              word: OpLower(Stack(Immed(11))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 6,
                      src_symb: 3,
                  ),
                  addr: (10),
              ),
              word: OpHigher(Control((Exit))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 9,
                      src_symb: 7,
                  ),
                  addr: (11),
              ),
              word: OpHigher(Math((
                  opcode: Mov,
                  args: RegToReg(Data, Accumulator),
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 10,
                      src_symb: 7,
                  ),
                  addr: (12),
              ),
              word: OpHigher(Math((
                  opcode: Mov,
                  args: RegMemToReg(Count, Accumulator),
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 11,
                      src_symb: 6,
                  ),
                  addr: (13),
              ),
              word: OpHigher(Branch((
                  opcode: Jz,
                  arg_higher: 0,
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 11,
                      src_symb: 6,
                  ),
                  addr: (14),
              ),
              word: OpLower(Branch((
                  arg_lower: 30,
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 14,
                      src_symb: 7,
                  ),
                  addr: (15),
              ),
              word: OpHigher(Alter((
                  opcode: Inc,
                  arg: Data,
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 15,
                      src_symb: 7,
                  ),
                  addr: (16),
              ),
              word: OpHigher(Math((
                  opcode: Mov,
                  args: RegMemToReg(Accumulator, Data),
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 16,
                      src_symb: 7,
                  ),
                  addr: (17),
              ),
              word: OpHigher(Math((
                  opcode: Mov,
                  args: RegImmed(Base, 0),
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 16,
                      src_symb: 7,
                  ),
                  addr: (18),
              ),
              word: OpLower(Math(RegImmed(4))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 19,
                      src_symb: 7,
                  ),
                  addr: (19),
              ),
              word: OpHigher(Io((
                  opcode: Out,
                  arg: 1,
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 21,
                      src_symb: 7,
                  ),
                  addr: (20),
              ),
              word: OpHigher(Alter((
                  opcode: Dec,
                  arg: Count,
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 22,
                      src_symb: 6,
                  ),
                  addr: (21),
              ),
              word: OpHigher(Branch((
                  opcode: Jz,
                  arg_higher: 0,
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 22,
                      src_symb: 6,
                  ),
                  addr: (22),
              ),
              word: OpLower(Branch((
                  arg_lower: 30,
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 24,
                      src_symb: 7,
                  ),
                  addr: (23),
              ),
              word: OpHigher(Alter((
                  opcode: Dec,
                  arg: Base,
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 25,
                      src_symb: 6,
                  ),
                  addr: (24),
              ),
              word: OpHigher(Branch((
                  opcode: Jz,
                  arg_higher: 0,
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 25,
                      src_symb: 6,
                  ),
                  addr: (25),
              ),
              word: OpLower(Branch((
                  arg_lower: 15,
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 27,
                      src_symb: 7,
                  ),
                  addr: (26),
              ),
              word: OpHigher(Math((
                  opcode: Shr,
                  args: RegImmed(Accumulator, 0),
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 27,
                      src_symb: 7,
                  ),
                  addr: (27),
              ),
              word: OpLower(Math(RegImmed(8))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 28,
                      src_symb: 7,
                  ),
                  addr: (28),
              ),
              word: OpHigher(Branch((
                  opcode: Jmp,
                  arg_higher: 0,
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 28,
                      src_symb: 7,
                  ),
                  addr: (29),
              ),
              word: OpLower(Branch((
                  arg_lower: 19,
              ))),
          ),
          (
              debug_info: (
                  src_info: (
                      src_line: 31,
                      src_symb: 3,
                  ),
                  addr: (30),
              ),
              word: OpHigher(Stack((
                  opcode: Ret,
                  args: None,
              ))),
          ),
      ],
      entrypoint: (6),
  )
```

Вывод виртуальной машины:
```
Hello, world!
```

Журнал состояний процессора (приведён для каждого начала цикла исполнения инструкции)
```
  [DEBUG lab3_rust::machine] Starting virtual machine
  [DEBUG lab3_rust::machine::control_unit] TICK:    9; REGS: eax:0x00000000, ebx:0x00000000, ecx:0x00000000, edx:0x00000000, eip:0x00000007, esp:0x00001000; FLAGS: F|F|F; INSTR: OpHigher(Math(MathOpHigher { opcode: Mov, args: RegImmed(Accumulator, 0) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:   19; REGS: eax:0x00000001, ebx:0x00000000, ecx:0x00000000, edx:0x00000000, eip:0x00000009, esp:0x00001000; FLAGS: F|F|F; INSTR: OpHigher(Stack(StackOpHigher { opcode: Call, args: Immed(0) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:   26; REGS: eax:0x00000001, ebx:0x00000000, ecx:0x00000000, edx:0x00000000, eip:0x0000000b, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Math(MathOpHigher { opcode: Mov, args: RegToReg(Data, Accumulator) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:   31; REGS: eax:0x00000001, ebx:0x00000000, ecx:0x00000000, edx:0x00000001, eip:0x0000000c, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Math(MathOpHigher { opcode: Mov, args: RegMemToReg(Count, Accumulator) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:   43; REGS: eax:0x00000001, ebx:0x00000000, ecx:0x0000000d, edx:0x00000001, eip:0x0000000e, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:   48; REGS: eax:0x00000001, ebx:0x00000000, ecx:0x0000000d, edx:0x00000001, eip:0x0000000f, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Alter(AlterOpWord { opcode: Inc, arg: Data })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:   53; REGS: eax:0x00000001, ebx:0x00000000, ecx:0x0000000d, edx:0x00000002, eip:0x00000010, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Math(MathOpHigher { opcode: Mov, args: RegMemToReg(Accumulator, Data) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:   65; REGS: eax:0x6c6c6548, ebx:0x00000000, ecx:0x0000000d, edx:0x00000002, eip:0x00000012, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Math(MathOpHigher { opcode: Mov, args: RegImmed(Base, 0) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:   70; REGS: eax:0x6c6c6548, ebx:0x00000004, ecx:0x0000000d, edx:0x00000002, eip:0x00000013, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Io(IoOpWord { opcode: Out, arg: 1 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:   75; REGS: eax:0x6c6c6548, ebx:0x00000004, ecx:0x0000000d, edx:0x00000002, eip:0x00000014, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Count })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:   85; REGS: eax:0x6c6c6548, ebx:0x00000004, ecx:0x0000000c, edx:0x00000002, eip:0x00000016, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:   90; REGS: eax:0x6c6c6548, ebx:0x00000004, ecx:0x0000000c, edx:0x00000002, eip:0x00000017, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Base })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  100; REGS: eax:0x6c6c6548, ebx:0x00000003, ecx:0x0000000c, edx:0x00000002, eip:0x00000019, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  110; REGS: eax:0x6c6c6548, ebx:0x00000003, ecx:0x0000000c, edx:0x00000002, eip:0x0000001b, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Math(MathOpHigher { opcode: Shr, args: RegImmed(Accumulator, 0) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  120; REGS: eax:0x006c6c65, ebx:0x00000003, ecx:0x0000000c, edx:0x00000002, eip:0x0000001d, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jmp, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  124; REGS: eax:0x006c6c65, ebx:0x00000003, ecx:0x0000000c, edx:0x00000002, eip:0x00000013, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Io(IoOpWord { opcode: Out, arg: 1 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  129; REGS: eax:0x006c6c65, ebx:0x00000003, ecx:0x0000000c, edx:0x00000002, eip:0x00000014, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Count })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  139; REGS: eax:0x006c6c65, ebx:0x00000003, ecx:0x0000000b, edx:0x00000002, eip:0x00000016, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  144; REGS: eax:0x006c6c65, ebx:0x00000003, ecx:0x0000000b, edx:0x00000002, eip:0x00000017, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Base })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  154; REGS: eax:0x006c6c65, ebx:0x00000002, ecx:0x0000000b, edx:0x00000002, eip:0x00000019, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  164; REGS: eax:0x006c6c65, ebx:0x00000002, ecx:0x0000000b, edx:0x00000002, eip:0x0000001b, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Math(MathOpHigher { opcode: Shr, args: RegImmed(Accumulator, 0) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  174; REGS: eax:0x00006c6c, ebx:0x00000002, ecx:0x0000000b, edx:0x00000002, eip:0x0000001d, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jmp, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  178; REGS: eax:0x00006c6c, ebx:0x00000002, ecx:0x0000000b, edx:0x00000002, eip:0x00000013, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Io(IoOpWord { opcode: Out, arg: 1 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  183; REGS: eax:0x00006c6c, ebx:0x00000002, ecx:0x0000000b, edx:0x00000002, eip:0x00000014, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Count })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  193; REGS: eax:0x00006c6c, ebx:0x00000002, ecx:0x0000000a, edx:0x00000002, eip:0x00000016, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  198; REGS: eax:0x00006c6c, ebx:0x00000002, ecx:0x0000000a, edx:0x00000002, eip:0x00000017, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Base })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  208; REGS: eax:0x00006c6c, ebx:0x00000001, ecx:0x0000000a, edx:0x00000002, eip:0x00000019, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  218; REGS: eax:0x00006c6c, ebx:0x00000001, ecx:0x0000000a, edx:0x00000002, eip:0x0000001b, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Math(MathOpHigher { opcode: Shr, args: RegImmed(Accumulator, 0) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  228; REGS: eax:0x0000006c, ebx:0x00000001, ecx:0x0000000a, edx:0x00000002, eip:0x0000001d, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jmp, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  232; REGS: eax:0x0000006c, ebx:0x00000001, ecx:0x0000000a, edx:0x00000002, eip:0x00000013, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Io(IoOpWord { opcode: Out, arg: 1 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  237; REGS: eax:0x0000006c, ebx:0x00000001, ecx:0x0000000a, edx:0x00000002, eip:0x00000014, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Count })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  247; REGS: eax:0x0000006c, ebx:0x00000001, ecx:0x00000009, edx:0x00000002, eip:0x00000016, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  252; REGS: eax:0x0000006c, ebx:0x00000001, ecx:0x00000009, edx:0x00000002, eip:0x00000017, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Base })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  262; REGS: eax:0x0000006c, ebx:0x00000000, ecx:0x00000009, edx:0x00000002, eip:0x00000019, esp:0x00000fff; FLAGS: F|T|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  266; REGS: eax:0x0000006c, ebx:0x00000000, ecx:0x00000009, edx:0x00000002, eip:0x0000000f, esp:0x00000fff; FLAGS: F|T|T; INSTR: OpHigher(Alter(AlterOpWord { opcode: Inc, arg: Data })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  271; REGS: eax:0x0000006c, ebx:0x00000000, ecx:0x00000009, edx:0x00000003, eip:0x00000010, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Math(MathOpHigher { opcode: Mov, args: RegMemToReg(Accumulator, Data) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  283; REGS: eax:0x77202c6f, ebx:0x00000000, ecx:0x00000009, edx:0x00000003, eip:0x00000012, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Math(MathOpHigher { opcode: Mov, args: RegImmed(Base, 0) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  288; REGS: eax:0x77202c6f, ebx:0x00000004, ecx:0x00000009, edx:0x00000003, eip:0x00000013, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Io(IoOpWord { opcode: Out, arg: 1 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  293; REGS: eax:0x77202c6f, ebx:0x00000004, ecx:0x00000009, edx:0x00000003, eip:0x00000014, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Count })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  303; REGS: eax:0x77202c6f, ebx:0x00000004, ecx:0x00000008, edx:0x00000003, eip:0x00000016, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  308; REGS: eax:0x77202c6f, ebx:0x00000004, ecx:0x00000008, edx:0x00000003, eip:0x00000017, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Base })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  318; REGS: eax:0x77202c6f, ebx:0x00000003, ecx:0x00000008, edx:0x00000003, eip:0x00000019, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  328; REGS: eax:0x77202c6f, ebx:0x00000003, ecx:0x00000008, edx:0x00000003, eip:0x0000001b, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Math(MathOpHigher { opcode: Shr, args: RegImmed(Accumulator, 0) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  338; REGS: eax:0x0077202c, ebx:0x00000003, ecx:0x00000008, edx:0x00000003, eip:0x0000001d, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jmp, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  342; REGS: eax:0x0077202c, ebx:0x00000003, ecx:0x00000008, edx:0x00000003, eip:0x00000013, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Io(IoOpWord { opcode: Out, arg: 1 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  347; REGS: eax:0x0077202c, ebx:0x00000003, ecx:0x00000008, edx:0x00000003, eip:0x00000014, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Count })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  357; REGS: eax:0x0077202c, ebx:0x00000003, ecx:0x00000007, edx:0x00000003, eip:0x00000016, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  362; REGS: eax:0x0077202c, ebx:0x00000003, ecx:0x00000007, edx:0x00000003, eip:0x00000017, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Base })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  372; REGS: eax:0x0077202c, ebx:0x00000002, ecx:0x00000007, edx:0x00000003, eip:0x00000019, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  382; REGS: eax:0x0077202c, ebx:0x00000002, ecx:0x00000007, edx:0x00000003, eip:0x0000001b, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Math(MathOpHigher { opcode: Shr, args: RegImmed(Accumulator, 0) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  392; REGS: eax:0x00007720, ebx:0x00000002, ecx:0x00000007, edx:0x00000003, eip:0x0000001d, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jmp, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  396; REGS: eax:0x00007720, ebx:0x00000002, ecx:0x00000007, edx:0x00000003, eip:0x00000013, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Io(IoOpWord { opcode: Out, arg: 1 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  401; REGS: eax:0x00007720, ebx:0x00000002, ecx:0x00000007, edx:0x00000003, eip:0x00000014, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Count })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  411; REGS: eax:0x00007720, ebx:0x00000002, ecx:0x00000006, edx:0x00000003, eip:0x00000016, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  416; REGS: eax:0x00007720, ebx:0x00000002, ecx:0x00000006, edx:0x00000003, eip:0x00000017, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Base })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  426; REGS: eax:0x00007720, ebx:0x00000001, ecx:0x00000006, edx:0x00000003, eip:0x00000019, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  436; REGS: eax:0x00007720, ebx:0x00000001, ecx:0x00000006, edx:0x00000003, eip:0x0000001b, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Math(MathOpHigher { opcode: Shr, args: RegImmed(Accumulator, 0) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  446; REGS: eax:0x00000077, ebx:0x00000001, ecx:0x00000006, edx:0x00000003, eip:0x0000001d, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jmp, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  450; REGS: eax:0x00000077, ebx:0x00000001, ecx:0x00000006, edx:0x00000003, eip:0x00000013, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Io(IoOpWord { opcode: Out, arg: 1 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  455; REGS: eax:0x00000077, ebx:0x00000001, ecx:0x00000006, edx:0x00000003, eip:0x00000014, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Count })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  465; REGS: eax:0x00000077, ebx:0x00000001, ecx:0x00000005, edx:0x00000003, eip:0x00000016, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  470; REGS: eax:0x00000077, ebx:0x00000001, ecx:0x00000005, edx:0x00000003, eip:0x00000017, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Base })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  480; REGS: eax:0x00000077, ebx:0x00000000, ecx:0x00000005, edx:0x00000003, eip:0x00000019, esp:0x00000fff; FLAGS: F|T|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  484; REGS: eax:0x00000077, ebx:0x00000000, ecx:0x00000005, edx:0x00000003, eip:0x0000000f, esp:0x00000fff; FLAGS: F|T|T; INSTR: OpHigher(Alter(AlterOpWord { opcode: Inc, arg: Data })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  489; REGS: eax:0x00000077, ebx:0x00000000, ecx:0x00000005, edx:0x00000004, eip:0x00000010, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Math(MathOpHigher { opcode: Mov, args: RegMemToReg(Accumulator, Data) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  501; REGS: eax:0x646c726f, ebx:0x00000000, ecx:0x00000005, edx:0x00000004, eip:0x00000012, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Math(MathOpHigher { opcode: Mov, args: RegImmed(Base, 0) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  506; REGS: eax:0x646c726f, ebx:0x00000004, ecx:0x00000005, edx:0x00000004, eip:0x00000013, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Io(IoOpWord { opcode: Out, arg: 1 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  511; REGS: eax:0x646c726f, ebx:0x00000004, ecx:0x00000005, edx:0x00000004, eip:0x00000014, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Count })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  521; REGS: eax:0x646c726f, ebx:0x00000004, ecx:0x00000004, edx:0x00000004, eip:0x00000016, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  526; REGS: eax:0x646c726f, ebx:0x00000004, ecx:0x00000004, edx:0x00000004, eip:0x00000017, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Base })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  536; REGS: eax:0x646c726f, ebx:0x00000003, ecx:0x00000004, edx:0x00000004, eip:0x00000019, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  546; REGS: eax:0x646c726f, ebx:0x00000003, ecx:0x00000004, edx:0x00000004, eip:0x0000001b, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Math(MathOpHigher { opcode: Shr, args: RegImmed(Accumulator, 0) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  556; REGS: eax:0x00646c72, ebx:0x00000003, ecx:0x00000004, edx:0x00000004, eip:0x0000001d, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jmp, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  560; REGS: eax:0x00646c72, ebx:0x00000003, ecx:0x00000004, edx:0x00000004, eip:0x00000013, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Io(IoOpWord { opcode: Out, arg: 1 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  565; REGS: eax:0x00646c72, ebx:0x00000003, ecx:0x00000004, edx:0x00000004, eip:0x00000014, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Count })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  575; REGS: eax:0x00646c72, ebx:0x00000003, ecx:0x00000003, edx:0x00000004, eip:0x00000016, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  580; REGS: eax:0x00646c72, ebx:0x00000003, ecx:0x00000003, edx:0x00000004, eip:0x00000017, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Base })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  590; REGS: eax:0x00646c72, ebx:0x00000002, ecx:0x00000003, edx:0x00000004, eip:0x00000019, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  600; REGS: eax:0x00646c72, ebx:0x00000002, ecx:0x00000003, edx:0x00000004, eip:0x0000001b, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Math(MathOpHigher { opcode: Shr, args: RegImmed(Accumulator, 0) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  610; REGS: eax:0x0000646c, ebx:0x00000002, ecx:0x00000003, edx:0x00000004, eip:0x0000001d, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jmp, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  614; REGS: eax:0x0000646c, ebx:0x00000002, ecx:0x00000003, edx:0x00000004, eip:0x00000013, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Io(IoOpWord { opcode: Out, arg: 1 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  619; REGS: eax:0x0000646c, ebx:0x00000002, ecx:0x00000003, edx:0x00000004, eip:0x00000014, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Count })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  629; REGS: eax:0x0000646c, ebx:0x00000002, ecx:0x00000002, edx:0x00000004, eip:0x00000016, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  634; REGS: eax:0x0000646c, ebx:0x00000002, ecx:0x00000002, edx:0x00000004, eip:0x00000017, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Base })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  644; REGS: eax:0x0000646c, ebx:0x00000001, ecx:0x00000002, edx:0x00000004, eip:0x00000019, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  654; REGS: eax:0x0000646c, ebx:0x00000001, ecx:0x00000002, edx:0x00000004, eip:0x0000001b, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Math(MathOpHigher { opcode: Shr, args: RegImmed(Accumulator, 0) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  664; REGS: eax:0x00000064, ebx:0x00000001, ecx:0x00000002, edx:0x00000004, eip:0x0000001d, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jmp, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  668; REGS: eax:0x00000064, ebx:0x00000001, ecx:0x00000002, edx:0x00000004, eip:0x00000013, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Io(IoOpWord { opcode: Out, arg: 1 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  673; REGS: eax:0x00000064, ebx:0x00000001, ecx:0x00000002, edx:0x00000004, eip:0x00000014, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Count })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  683; REGS: eax:0x00000064, ebx:0x00000001, ecx:0x00000001, edx:0x00000004, eip:0x00000016, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  688; REGS: eax:0x00000064, ebx:0x00000001, ecx:0x00000001, edx:0x00000004, eip:0x00000017, esp:0x00000fff; FLAGS: F|F|T; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Base })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  698; REGS: eax:0x00000064, ebx:0x00000000, ecx:0x00000001, edx:0x00000004, eip:0x00000019, esp:0x00000fff; FLAGS: F|T|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  702; REGS: eax:0x00000064, ebx:0x00000000, ecx:0x00000001, edx:0x00000004, eip:0x0000000f, esp:0x00000fff; FLAGS: F|T|T; INSTR: OpHigher(Alter(AlterOpWord { opcode: Inc, arg: Data })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  707; REGS: eax:0x00000064, ebx:0x00000000, ecx:0x00000001, edx:0x00000005, eip:0x00000010, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Math(MathOpHigher { opcode: Mov, args: RegMemToReg(Accumulator, Data) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  719; REGS: eax:0x00000021, ebx:0x00000000, ecx:0x00000001, edx:0x00000005, eip:0x00000012, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Math(MathOpHigher { opcode: Mov, args: RegImmed(Base, 0) })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  724; REGS: eax:0x00000021, ebx:0x00000004, ecx:0x00000001, edx:0x00000005, eip:0x00000013, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Io(IoOpWord { opcode: Out, arg: 1 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  729; REGS: eax:0x00000021, ebx:0x00000004, ecx:0x00000001, edx:0x00000005, eip:0x00000014, esp:0x00000fff; FLAGS: F|F|F; INSTR: OpHigher(Alter(AlterOpWord { opcode: Dec, arg: Count })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  739; REGS: eax:0x00000021, ebx:0x00000004, ecx:0x00000000, edx:0x00000005, eip:0x00000016, esp:0x00000fff; FLAGS: F|T|T; INSTR: OpHigher(Branch(BranchOpHigher { opcode: Jz, arg_higher: 0 })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  743; REGS: eax:0x00000021, ebx:0x00000004, ecx:0x00000000, edx:0x00000005, eip:0x0000001e, esp:0x00000fff; FLAGS: F|T|T; INSTR: OpHigher(Stack(StackOpHigher { opcode: Ret, args: None })); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine::control_unit] TICK:  749; REGS: eax:0x00000021, ebx:0x00000004, ecx:0x00000000, edx:0x00000005, eip:0x0000000a, esp:0x00001000; FLAGS: F|T|T; INSTR: OpHigher(Control(ControlOpWord(Exit))); STATE: ExecuteInstruction { tick_count: 1 }
  [DEBUG lab3_rust::machine] Executed 100 instructions in 749 ticks
  [DEBUG lab3_rust::machine] Succesfully finished simulation
```

#### cat
[input-file](./tests/inputs/cat.yml)

[snapshot-file](./tests/snapshots/integration__test@cat.yml.snap)
```asm
  start: 
    in 0
    cmp eax, 0  ;если встречаем NULL - заканчиваем программу
    jz end
    out 1
    jmp start
  end:
    exit
```

#### hello_user_name
[input-file](./tests/inputs/hello_user_name.yml)

[snapshot-file](./tests/snapshots/integration__test@hello_user_name.yml.snap)
```asm
  prompt: word "What is your name?"
  hello_start: word "Hello, "
  hello_end: word "!"
  username_buf: word 0 word 0 word 0 word 0

  start: 
    mov eax, prompt
    call print_string
    call print_newline

    mov eax, username_buf
    call read_string

    mov eax, hello_start
    call print_string

    mov eax, username_buf
    call print_string

    mov eax, hello_end
    call print_string

    exit


  read_string:      ;EAX - указатель на буфер
    push eax        ;сохраняем указатель на начало строки (ячейка для длины)
    mov edx, eax
    mov ecx, 0      ;используем ecx для хранения длины прочитанной строки

  read_string_main_loop:
    inc edx         ;смещяем edx на ячейку для следующей четвёрки символов
    mov ebx, 0      ;сбрасываем ebx - используем его как индекс по байтам ячейки (0 - 3)

  read_string_word_loop:
    mov eax, 0      ;читаем символ
    in 0
    cmp eax, 0      ;если NULL - оканчиваем чтение
    jz read_string_end
    inc ecx         ;увеличиваем длину строки
    push ebx        ;сохраняем текущий индекс в ячейке
  read_string_shift_loop:
    cmp ebx, 0      ;если индекс равен нулю - сохраняем символ в ячейку
    jz read_string_word_stuff
    shl eax, 8      ;иначе - смещяем символ влево на одну позицию
    dec ebx         ;и уменьшаем индекс "отступа"
    jmp read_string_shift_loop
  read_string_word_stuff:
    add [edx], eax  ;сохраняем символ в ячейку
    pop ebx         ;восстанавливаем индекс этого символа
    inc ebx         ;подготавливаем индекс для следующего символа
    cmp ebx, 4      ;если место в ячейке кончилось - переходим к следующей
    jz read_string_main_loop
    jmp read_string_word_loop

  read_string_end:
    pop eax         ;восстанавливаем указатель на начало строки
    mov [eax], ecx  ;записываем размер строки
    ret


  print_newline:
    mov eax, 10
    out 1
    ret


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
```

#### prob2
[input-file](./tests/inputs/prob2.yml)

[snapshot-file](./tests/snapshots/integration__test@prob2.yml.snap)
```asm
  num: word 1         ;буфер для предпоследнего числа последовательности
  result: word 2      ;буфер для результата

  start:
    mov ecx, 3        ;счётчик до следующего чётного числа
    mov eax, 2        ;последнее число в последовательности
  loop:
    mov edx, eax      ;сохраняем последнее число
    add eax, [num]    ;рассчитываем следующее число
    mov [num], edx    ;обновляем предпоследнее число
    dec ecx           ;уменьшаем счётчик. Если до чётного числа ещё не дошли - повторяем процесс
    jnz loop
    mov ecx, 3        ;сбрасываем счётчик
    cmp eax, 4000000  ;если достигли предела - завершаем работу
    jns end
    add [result], eax ;обновляем результат
    jmp loop

  end:
    mov eax, [result]
    call print_uint
    exit


  print_uint:       ;EAX - число
    mov ebx, 10     ;записываем делитель в ebx
    mov ecx, esp    ;сохраняем текущую позицию в стэке

  print_uint_loop:
    dec esp         ;выделяем место под символ
    mov edx, 0      ;очищаем edx
    div             ;делим eax на ebx: в eax - частное, в edx - остаток
    add edx, 48     ;"смещаем" остаток для получения ASCII-цифры
    mov [esp], edx  ;сохраняем получившийся символ
    cmp eax, 0      ;если ещё остались разряды числа - повторяем процесс
    jnz print_uint_loop
  print_uint_print:
    mov eax, [esp]  ;читаем старший невыведенный символ
    out 1           ;выводим его
    inc esp         ;освобождаем память с выведенным символом
    cmp esp, ecx    ;если символы не кончились - повторяем процесс
    jnz print_uint_print
  print_uint_end:
    ret
```

| ФИО | <алг> | <LoC> | <code байт> | <code инстр.> | <инстр.> | <такт.> | <вариант> |
|-|-|-|-|-|-|-|-|
Хайкин Олег Игоревич | hello | 91 | 396 | 99 | 341 | 2658 | asm \| cisc \| neum \| hw \| tick \| struct \| stream \| port \| pstr \| prob2 \| \[4\]char |
Хайкин Олег Игоревич | cat | 8 | 36 | 9 | 39 | 301 | asm \| cisc \| neum \| hw \| tick \| struct \| stream \| port \| pstr \| prob2 \| \[4\]char |
Хайкин Олег Игоревич | prob2 | 44 | 200 | 50 | 310 | 4932 (из них ~2000 на сам алгоритм, остальное для вывода результата) | asm \| cisc \| neum \| hw \| tick \| struct \| stream \| port \| pstr \| prob2 \| \[4\]char |