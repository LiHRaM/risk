use std::io;
use rand::prelude::*;
use std::cmp::min;

fn get_number() -> io::Result<u8> {
    let mut num = String::new();
    io::stdin().read_line(&mut num)?;
    let num: u8 = num.trim().parse().expect("Invalid input");
    Ok(num)
}

fn should_attack() -> io::Result<bool> {
    let mut buffer = String::new();
    println!("Attack? (y/n)");
    io::stdin().read_line(&mut buffer)?;

    Ok(
        match buffer.trim() {
            "y" => true,
            _   => {
                println!("{}", buffer);
                false
            },
        }
    )
}

fn get_roll() -> u8 {
    rand::thread_rng().gen_range(1, 7)
}

struct Rolls {
    items: Vec<u8>
}

impl Rolls {
    fn new(army_size: u8) -> Rolls {
        let upper_bound = min(3, army_size);
        let mut items: Vec<u8> = (0..upper_bound).map(|_| get_roll()).collect();
        items.sort_unstable_by(|a, b| b.cmp(a));
        Rolls {
            items: items,
        }
    }
}

#[derive(Debug)]
struct Battle {
    attacker: u8,
    defender: u8,
}

impl Battle {
    fn new() -> io::Result<Battle> {
        println!("Please enter the size of attacker's army: ");
        let p1_size = get_number()?;
        println!("Please enter the size of defender's army: ");
        let p2_size = get_number()?;
        
        Ok(
            Battle {
                attacker: p1_size,
                defender: p2_size,
            }
        )
    }

    fn can_attack(&self) -> bool {
        self.attacker > 0 && self.defender > 0
    }

    fn attack(self) -> Battle {
        let attacker_rolls = Rolls::new(self.attacker);
        let defender_rolls = Rolls::new(self.defender);

        let mut attacker = self.attacker;
        let mut defender = self.defender;

        let bound = min(attacker_rolls.items.len(), defender_rolls.items.len());

        println!("{:?} {:?}", attacker_rolls.items, defender_rolls.items);

        for i in 0..bound {
            if attacker_rolls.items[i] > defender_rolls.items[i] {
                defender -= 1;
                
            } else {
                attacker -= 1;
            }
        }

        Battle {
            attacker,
            defender,
        }
    }
}

fn main() -> io::Result<()> {
    let mut battle = Battle::new()?;
    println!("{:?}", battle);
    while battle.can_attack() && should_attack()? {
        battle = battle.attack();
        println!("{:?}", battle);
    };
    Ok(())
}