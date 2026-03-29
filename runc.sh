CLASSPATH=$(sbt --batch 'export runtime:fullClasspath' 2>/dev/null | tail -1 | sed 's/\\/\//g')
java -classpath "$CLASSPATH" com.wolfskeep.CompiledUniversalMachine "$@"

