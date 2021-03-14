package mygit

import com.monovore.decline._

object App extends CommandApp(
  name = "mygit",
  header = "A git clone in Scala",
  main = {
    Opts.subcommands(Commands.init, Commands.commit, Commands.log).orElse(Opts.help)
  }
)