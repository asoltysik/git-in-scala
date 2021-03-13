package mygit

import com.monovore.decline.Command
import com.monovore.decline.Opts
import java.io.File

object Commands {

    val init = Command("init", "Initialize git-in-scala repository in the current directory")(Opts {
        println("Initializing git-in-scala repository.")

        new File(".git-in-scala/objects").mkdirs()
        new File(".git-in-scala/refs/heads").mkdirs()
    })

}