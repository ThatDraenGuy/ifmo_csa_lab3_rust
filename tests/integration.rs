use std::{
    error::Error,
    fs::{self, File},
    io::Read,
    path::Path,
};

use gag::BufferRedirect;
use insta::{assert_ron_snapshot, glob};
use lab3_rust::{machine, translator};
use serde::{Deserialize, Serialize};
use tempfile::{tempdir, TempDir};

#[derive(Deserialize)]
struct TestInput {
    translator_input: String,
    machine_input: String,
}

#[derive(Serialize)]
struct TestOutput {
    translator_output: String,
    stdout: String,
}

fn perform_test(temp_dir: &TempDir, input_path: &Path) -> Result<TestOutput, Box<dyn Error>> {
    // Достаём имя теста
    let test_name = input_path.file_stem().unwrap().to_str().unwrap();

    // Читаем входные данные
    let input_file = File::open(input_path)?;
    let input: TestInput = serde_yaml::from_reader(input_file)?;

    // Создаём временные файлы для входных и выходных данных
    let translator_input = temp_dir.path().join(test_name.to_owned() + "__translator_in");
    let machine_input = temp_dir.path().join(test_name.to_owned() + "__machine_in");
    let target = temp_dir.path().join(test_name.to_owned() + "__target");

    // Копируем входные данные
    fs::write(&translator_input, input.translator_input)?;
    fs::write(&machine_input, input.machine_input)?;

    // Хватаем вывод в STDOUT
    let mut stdout_buf = BufferRedirect::stdout()?;
    // Запускаем транслятор и виртуальную машину
    translator::main(&translator_input, &target)?;
    machine::main(&target, &machine_input)?;

    // Читаем выходные данные
    let mut stdout = String::new();
    stdout_buf.read_to_string(&mut stdout)?;

    // Читаем результат работы транслятора
    let translator_output = fs::read_to_string(target)?;

    //Возвращаем результат
    Ok(TestOutput { translator_output, stdout })
}

#[test]
fn test() {
    // Создаём одну временную директорию для всех файлов
    let dir = tempdir().unwrap();
    glob!("inputs/*.ron", |input_path| {
        // Проходимся по всем файлам с входными данными и для каждого из них создаём снапшот
        let result = perform_test(&dir, input_path).unwrap();
        assert_ron_snapshot!(result);
    })
}
