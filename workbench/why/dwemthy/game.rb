require './rabbit.rb'
require './foes.rb'

r = Rabbit.new
dwarr = DwemthysArray[IndustrialRaverMonkey.new,
                      DwarvenAngel.new,
                      AssistantViceTentacleAndOmbudsman.new,
                      TeethDeer.new,
                      IntrepidDecomposedCyclist.new,
                      Dragon.new]


while r.life > 0 and not dwarr.empty?
    if r.life < 15
        r % dwarr
    elsif dwarr.life > 500
        r * dwarr
    else
        r / dwarr
    end
end

if r.life <= 0
    puts "Your rabbit has died. GAME OVER."
else
    puts "You win!"
end
