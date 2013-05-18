#!/usr/bin/env ruby


class TodoList < Array
    def self.load(file)
        # read the file, create a list, create items, add them
        tdl = TodoList.new
        File.read(file).each_line do |line|
            tdl << line.chomp
        end
        tdl
    end

    def add(todo)
        if todo.is_a?(TodoItem)
            push(todo)
        else
            push(TodoItem.new(todo))
        end
    end

    alias_method :<<, :add

    def mark_done(id)
        self[id].done = true
    end

    def write(file)
        # write the file, only write the undone items
        File.open(file, 'w') do |f|
            f.write(reject {|item| item.done?}.join("\n"))
        end
    end
end


class TodoItem
    # provide reader and setter for name and state
    attr_accessor :name, :done
    alias_method :done?, :done

    def initialize(name)
        # store name
        @name = name
        # set state to undone
        @done = false
    end

    def to_s
        @name
    end
end


class CommandLineInterface
    def initialize
        @list = TodoList.load("todo.td")
    end

    def done
        @list.mark_done ARGV[1].to_i
    end

    def add
        @list << ARGV[1..-1].join(" ")
    end

    def write
        @list.write("todo.td")
    end

    def list
        @list.each_with_index do |line, index|
            puts "##{index} - #{line.name.capitalize}"
        end
    end
end


if __FILE__ == $0
    cli = CommandLineInterface.new
    cli.send ARGV[0]
    cli.write
end
