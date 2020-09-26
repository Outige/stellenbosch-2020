import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.Mixer;

public class Main {
    public static Mixer mixer;
    public static Clip clip;
    public static void main(String[] args) {
        Log log = new Log(Log.Level.WARNING);
        
        /* garbage print of mixInfos */
        Mixer.Info[] mixInfos = AudioSystem.getMixerInfo();
        for (Mixer.Info info : mixInfos) {
            log.log(Log.Level.GARBAGE, info.getName() + "; " + info.getDescription());
        }

        mixer = AudioSystem.getMixer(mixInfos[0]);

        DataLine.Info dataInfo = new DataLine.Info(Clip.class, null);
        try {
            clip = (Clip) mixer.getLine(dataInfo);
        } catch (Exception e) {
            log.log(Log.Level.DONGOOFED, e.toString());
        }
    }
}
