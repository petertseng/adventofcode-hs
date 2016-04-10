require_relative '../adventofcode-common/test'

test_and_exit { |daypad|
  commands = Dir.glob("#{__dir__}/bin/#{daypad}*.hs").flat_map { |haskell|
    name = File.basename(haskell, '.hs')
    Dir.glob("#{__dir__}/dist-newstyle/build/**/build/#{name}/#{name}")
  }
  next if commands.empty?
  raise "Need exactly one command not #{commands}" if commands.size != 1
  commands[0]
}
