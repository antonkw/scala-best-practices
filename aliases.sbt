import Util._

addCommandAlias("cd", "project")
addCommandAlias("root", "cd scala-best-practices")
addCommandAlias("c", "compile")
addCommandAlias("ca", "test:compile")
addCommandAlias("t", "test")
addCommandAlias("r", "run")
addCommandAlias(
  "up2date",
  "reload plugins; dependencyUpdates; reload return; dependencyUpdates"
)

onLoadMessage +=
  s"""|
      |───────────────────────────
      |  List of defined ${styled("aliases")}
      |────────┬──────────────────
      |${styled("cd")}      │ project
      |${styled("root")}    │ cd scala-best-practices
      |${styled("c")}       │ compile
      |${styled("ca")}      │ compile all
      |${styled("t")}       │ test
      |${styled("r")}       │ run
      |${styled("up2date")} │ dependencyUpdates
      |────────┴──────────────────""".stripMargin