use std::fmt;
use std::process::Command;
use std::{error::Error, thread, time::Duration};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use signal_hook::{consts::SIGINT, consts::SIGHUP, iterator::Signals};

#[derive(Debug)]
struct WeirdOutputError {
    details: String
}

impl WeirdOutputError {
    fn new(msg: &str) -> WeirdOutputError {
        WeirdOutputError{details: msg.to_string()}
    }
}

impl fmt::Display for WeirdOutputError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.details)
    }

}

impl Error for WeirdOutputError {
    fn description(&self) -> &str {
        &self.details
    }
}


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
    if percentage == "" {
        return Err(Box::new(WeirdOutputError::new("weird upower output")));
    }
    Ok(format!("BATT {} {}", percentage.trim(), state))
}

fn volume() -> Result<String, Box<dyn Error>> {
    let wpctl = Command::new("wpctl").arg("get-volume").arg("@DEFAULT_AUDIO_SINK@").output()?;
    let s = String::from_utf8(wpctl.stdout)?;
    if !s.contains("Volume: ") {
        return Err(Box::new(WeirdOutputError::new("weird wpctl output")));
    }
    let level = String::from(s.split(" ").nth(1).unwrap());
    let mut muted = String::from("");
    if s.contains("MUTED") {
        muted = String::from("[M]");
    }

    Ok(format!("VOL {}{}", level.trim(), muted))
}

fn wifi() -> Result<String, Box<dyn Error>> {
    let nmcli = Command::new("nmcli").arg("device").arg("show").arg("wlp1s0").output()?;
    let mut state = String::from("");
    for l in String::from_utf8(nmcli.stdout)?.lines() {
        // TODO replac (connected) with a checkmark or something
        if l.contains("GENERAL.STATE") {
            state = String::from(
                String::from(l.split(":").last().unwrap()).trim());
            break;
        }
    }
    Ok(String::from(format!("WIFI {}", state)))
}

fn datetime() -> Result<String, Box<dyn Error>> {
    let date = Command::new("date").arg("+%Y-%m-%d %H:%M").output()?;
    let s = String::from_utf8(date.stdout)?;
    Ok(String::from(s.trim()))
}

fn output() -> String {
    let vol = match volume() {
        Ok(s) => s,
        Err(_) => String::from("D: VOL")
    };
    let batt = match power() {
        Ok(s) => s,
        Err(_) => String::from("D: BATT")
    };
    let wf = match wifi() {
        Ok(s) => s,
        Err(_) => String::from("D: WIFI")
    };
    let dt = match datetime() {
        Ok(s) => s,
        Err(_) => String::from("D: DATE")
    };
    return format!("[{}] [{}] [{}] {} ", vol, batt, wf, dt);
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
