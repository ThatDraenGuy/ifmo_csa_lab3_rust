use std::{
    error::Error,
    fs::{self, File},
    io::Read,
    path::Path,
};

use gag::BufferRedirect;
use insta::{assert_snapshot, glob};
use lab3_rust::{machine, translator};
use serde::{Deserialize, Serialize};
use tempfile::{tempdir, TempDir};

#[derive(Deserialize)]
struct TestInput {
    translator_input: String,
    machine_input: String,
}

#[derive(Debug, Serialize)]
struct TestOutput {
    pub translator_output: String,
    pub translator_result: String,
    pub stdout: String,
    pub machine_result: String,
    pub log: String,
}

fn perform_test(temp_dir: &TempDir, input_path: &Path) -> Result<String, Box<dyn Error>> {
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
    fs::write(&translator_input, &input.translator_input)?;
    fs::write(&machine_input, &input.machine_input)?;

    // Хватаем вывод в STDOUT и STDERR
    let mut stdout_buf = BufferRedirect::stdout()?;
    let mut stderr_buf = BufferRedirect::stderr()?;
    // Запускаем транслятор и виртуальную машину
    let translator_res = translator::main(&translator_input, &target);
    let machine_res = machine::main(&target, &machine_input);

    let translator_result = match &translator_res {
        Ok(_) => "OK".to_owned(),
        Err(e) => e.to_string(),
    };
    let machine_result = match machine_res {
        Ok(_) => "OK".to_owned(),
        Err(e) => e.to_string(),
    };

    // Читаем выходные данные
    let mut stdout = String::new();
    stdout_buf.read_to_string(&mut stdout)?;

    let mut log = String::new();
    stderr_buf.read_to_string(&mut log)?;

    // Читаем результат работы транслятора
    let translator_output = match &translator_res {
        Ok(_) => fs::read_to_string(target)?,
        Err(_) => "".to_owned(),
    };

    //Возвращаем результат
    let output = TestOutput { translator_output, translator_result, stdout, machine_result, log };

    // Сериализуем в yaml "ручками" вместо использования assert_yaml_snapshot
    // чтобы получить развёрнутый вариант длинных строк
    Ok(serde_yaml::to_string(&output)?)
}

#[test]
fn test() {
    env_logger::builder()
        .target(env_logger::Target::Stderr)
        .format_timestamp(None)
        .filter_level(log::LevelFilter::Debug)
        .write_style(env_logger::WriteStyle::Never)
        .init();
    // Создаём одну временную директорию для всех файлов
    let dir = tempdir().unwrap();
    glob!("inputs/*.yml", |input_path| {
        // Проходимся по всем файлам с входными данными и для каждого из них создаём снапшот
        let result = perform_test(&dir, input_path).unwrap();
        assert_snapshot!(result);
    })
}
