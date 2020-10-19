public class Log {
    public enum Level { 
        GARBAGE, // really useless prints you refuse to remove
        DEBUG, // useful prints when debugging a problem
        VINFO, // verbose information
        INFO, // information
        NOTE, // this print indicate a future problem can occur
        WARNING, // not quite an error, but something isn't quite right
        ERROR, // an error but the code can still run
        DONGOOFED; // don goofed son
    }
    
    private Level level;

    Log(Level level) {
        this.level = level;
    }

    public void log(Level level, String message) {
        if (level.compareTo(this.level) >= 0) {
            System.out.println(level.toString() + ": " + message);
        }
    }
}
