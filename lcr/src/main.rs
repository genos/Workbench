//! A simulation of the game `LCR`.
use clap::Parser;
use rand::{prelude::*, SeedableRng};
use std::{fmt, iter, string, thread, time::Duration};
use tabled::{
    builder::Builder,
    settings::{object::Rows, style::Style, themes::Colorization, Color},
};

/// A simulation of the game `LCR`.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Number of players in the game
    #[arg(long, default_value_t = 4)]
    num_players: usize,
    /// Number of chips per player to start
    #[arg(long, default_value_t = 3)]
    chips_per: u32,
    /// Seed for randomization
    #[arg(long, default_value_t = 1729)]
    seed: u64,
    /// Sleep duration between founds
    #[arg(short, long, default_value_t = 1.5)]
    sleep: f64,
}

struct Game {
    round: u32,
    center: u32,
    players: Vec<u32>,
    turn: Option<usize>,
    won: bool,
    rng: SmallRng,
}

impl From<&Args> for Game {
    fn from(args: &Args) -> Self {
        Self {
            round: 1,
            center: 0,
            players: vec![args.chips_per; args.num_players],
            turn: Some(0),
            won: false,
            rng: SmallRng::seed_from_u64(args.seed),
        }
    }
}

impl fmt::Display for Game {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut table = Builder::from_iter(vec![
            ["Round #".to_string(), "Center".to_string()]
                .into_iter()
                .chain((0..self.players.len()).map(|i| format!("Player {}", i + 1)))
                .collect::<Vec<_>>(),
            [&self.round, &self.center]
                .into_iter()
                .chain(self.players.iter())
                .map(string::ToString::to_string)
                .collect::<Vec<_>>(),
        ])
        .build();
        let info_colors = iter::repeat_n(Color::BG_WHITE | Color::FG_BLACK, 2)
            .chain(self.players.iter().enumerate().map(|(i, &p)| {
                if self.won && p > 0 {
                    Color::BG_GREEN | Color::FG_BLACK
                } else if self.turn == Some(i) {
                    Color::BG_MAGENTA | Color::FG_BLACK
                } else if p == 0 {
                    Color::BG_RED | Color::FG_BLACK
                } else {
                    Color::BG_CYAN | Color::FG_BLACK
                }
            }))
            .collect::<Vec<_>>();
        table
            .with(Style::empty())
            .with(Colorization::exact(
                [Color::BG_WHITE | Color::FG_BLACK],
                Rows::first(),
            ))
            .with(Colorization::columns(info_colors))
            .modify(Rows::first(), Color::BG_BLUE | Color::FG_BRIGHT_WHITE);
        writeln!(f, "{table}")
    }
}

impl Game {
    fn next_turn(&mut self) {
        if let Some(t) = self.turn {
            let l = (t - 1) % self.players.len();
            let r = (t + 1) % self.players.len();
            let num_dice = match self.players.get(t).copied().unwrap_or_default() {
                i if i < 3 => i,
                _ => 3,
            };
            for _ in 0..num_dice {
                match self.rng.gen_range(0..=4) {
                    0 => {
                        print!("L");
                        self.players[t] = self.players[t].saturating_sub(1);
                        self.players[l] += 1;
                    }
                    1 => {
                        print!("C");
                        self.players[t] = self.players[t].saturating_sub(1);
                        self.center += 1;
                    }
                    2 => {
                        print!("R");
                        self.players[t] = self.players[t].saturating_sub(1);
                        self.players[r] += 1;
                    }
                    _ => {
                        print!("•");
                    }
                }
            }
            println!("\n");
            self.round += 1;
            match (
                (t + 1..self.players.len())
                    .chain(0..t)
                    .find(|&p| self.players[p] > 0),
                self.players.iter().filter(|&&p| p > 0).count(),
            ) {
                (None, _) => {
                    self.players[t] += self.center;
                    self.center = 0;
                    self.won = true;
                }
                (Some(t2), 1) => {
                    self.players[t2] += self.center;
                    self.center = 0;
                    self.won = true;
                    self.turn = None;
                }
                (s, _) => self.turn = s,
            }
        }
    }
}

fn main() {
    let args = Args::parse();
    let mut game = Game::from(&args);
    while !game.won {
        println!("{game}");
        game.next_turn();
        thread::sleep(Duration::from_secs_f64(args.sleep));
    }
    println!("{game}");
}
