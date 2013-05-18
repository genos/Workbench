require 'bindata'

class Rectangle < BinData::Record
    endian  :little
    uint16  :len
    string  :name, :read_length => :len
    uint32  :width
    uint32  :height
end

puts "Writing..."
File.open("test", "wb") do |io|
    r = Rectangle.new(:len => "graham's rect".length,
                      :name => "graham's rect",
                      :width => 17,
                      :height => 42)
    r.write(io)
end

puts "Reading..."
File.open("test", "rb") do |io|
    r = Rectangle.read(io)
    puts "Rectangle #{r.name} is #{r.width} x #{r.height}"
end

puts "Cleaning up..."
File.delete("test")
