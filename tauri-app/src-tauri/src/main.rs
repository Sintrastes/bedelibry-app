#![cfg_attr(
  all(not(debug_assertions), target_os = "windows"),
  windows_subsystem = "windows"
)]

use tauri::Manager;

fn main() {
  tauri::Builder::default()
    .setup(|app| { 
      app.get_window("main").unwrap()
          .eval("window.location.replace('http://127.0.0.1:3911')");
      Ok(())
    })
    .run(tauri::generate_context!())
    .expect("error while running tauri application");
}
