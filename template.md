# Разбор структуры репозитория
Данный шаблон предлагает структуру проекта, поддерживающую:
- сборку двух исполняемых файлов (machine и translator)
- настроенные форматтер (rustfmt) и линтер (clippy)
- настроенный модуль для интеграционного тестирования
- настроенный CI (Github Actions)

# Детальный разбор шаблона
## Структура модулей исходного кода
Исходный код [src](./src) можно разделить на 3 части:
1. [Библиотека](./src/lib.rs)
2. [Исполняемый файл транслятора](./src/bin/translator.rs)
3. [Исполняемый файл фиртуальной машины](./src/bin/machine.rs)

Библиотека состоит из 3-ёх модулей:
1. [isa](./src/isa.rs)
2. [translator](./src/translator.rs)
3. [machine](./src/machine.rs)

Модуль isa публичен в рамках библиотеки и предназначен для задания Instruction Set Architecture  
Модули translator и machine предназначены для использования соответствующими бинарниками.

### Почему так?
Данная структура была выбрана ввиду её минимализма. Я решил не разбивать проект на отдельные crate'ы (в духе использлвания корня как workspace'а с модулями isa, machine и translator), т.к. считаю такое разбиение overkill'ом для размеров проекта.  
Cargo не поддерживает несколько библиотек в рамках одного crate'а, поэтому собирается общая библиотека и для виртуальной машины, и для транслятора. Такой подход облегчает ввод интеграционных тестов для всей системы, и, учитывая тесную взаимосвязь этих модулей, я не считаю "единость" библиотеки большой проблемой.

## Тестирование
Для интеграционного тестирования используется библиотека [insta](https://crates.io/crates/insta), предоставляющий возможность тестирования на основе снапшотов.  
Я считаю эту библиотеку наилучшим аналогом golden-тестов в рамках Rust'а. Технически, снапшоты хранят в себе только результаты, а не входные данные, но это исправляется просто ручной обработкой файлов входных данных, настроенной мною.

## Форматтер и линтер
[Конфиг форматтера](./rustfmt.toml) и [конфиг линтера](./clippy.toml) приведены, скорее, для примера, чем из-за осознанной их конфигурации. 

## CI
[Workflow](./.github/workflows/rust.yml) настроен для Github Actions, и производит следующиие действия:
- Проверка форматирования
- Проверка линтером
- Тестирование
- Сборка

CI настроен для работы при выполнении push и pull request на ветках master и workflow