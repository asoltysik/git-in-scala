package mygit

import com.monovore.decline._

object App extends CommandApp(
  name = "mygit",
  header = "A git clone in Scala",
  main = {
    Opts.help
  }
)