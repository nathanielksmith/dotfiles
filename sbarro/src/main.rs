use std::process::Command;
use std::{error::Error, thread, time::Duration};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use signal_hook::{consts::SIGINT, consts::SIGHUP, iterator::Signals};

fn power() -> Result<String, Box<dyn Error>> {
    let upower = Command::new("upower").arg("-i").arg("/org/freedesktop/UPower/devices/battery_BAT1").output()?;
    let mut percentage = String::from("");
    let mut state = String::from("");
    for l in String::from_utf8(upower.stdout)?.lines() {
        if l.contains("percentage") {
            percentage = String::from(l.split(":").last().unwrap());
            continue;
        }
        if l.contains("state") {
            state = String::from(
                String::from(l.split(":").last().unwrap()).trim());
            if state == "discharging" {
                state = String::from("v");
            } else if state == "charging" {
                state = String::from("^");
            } else {
                state = String::from("-");
            }
            continue;
        }
    }
    Ok(format!("BATT {} {}", percentage.trim(), state.trim()))
}

// status_command while date +'%Y-%m-%d %H:%M '; do sleep 5; done

fn output() -> String {
    let batt = match power() {
        Ok(s) => s,
        Err(_) => String::from(":( BATT")
    };
    // TODO volume
    // TODO battery
    // TODO wifi
    return format!("{}", batt);
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut signals = Signals::new(&[SIGINT, SIGHUP])?;
    let quitting = Arc::new(AtomicBool::new(false));
    let qclone = quitting.clone();

    thread::spawn(move || {
        for sig in signals.forever() {
            match sig {
                SIGHUP => {
                    println!("{}", output());
                },
                SIGINT => {
                    println!("shutting down...");
                    qclone.store(true, Ordering::Relaxed);
                }
                _ => { }
                
            }
        }
    });

    while !quitting.load(Ordering::Relaxed) {
        println!("{}", output());
        thread::sleep(Duration::from_secs(5));
    }

    Ok(())
}
