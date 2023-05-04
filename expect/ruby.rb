#!/usr/bin/env ruby

require 'pty'
require 'expect'

pattern = /Average:\r\n[0-9](\.[0-9]*)?\r\n/

n = ARGV[0].to_f.to_i

PTY.spawn("./a.out") do |o, i, id|
    o.expect(/Number of numbers\?/) {|r| i.puts n}
    o.expect(/Numbers\?/) {n.times{i.puts rand}}
    puts o.expect(pattern)[0].match(pattern)
end
