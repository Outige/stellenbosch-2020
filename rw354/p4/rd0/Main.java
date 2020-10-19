import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.Mixer;
import java.net.URL;

public class Main {
    public static Mixer mixer;
    public static Clip clip;
    public static void main(String[] args) {
        Log log = new Log(Log.Level.GARBAGE);
        
        /* garbage print of mixInfos */
        Mixer.Info[] mixInfos = AudioSystem.getMixerInfo();
        for (Mixer.Info info : mixInfos) {
            log.log(Log.Level.GARBAGE, info.getName() + "; " + info.getDescription());
        }

        mixer = AudioSystem.getMixer(mixInfos[1]);

        DataLine.Info dataInfo = new DataLine.Info(Clip.class, null);
        try {
            clip = (Clip) mixer.getLine(dataInfo);
        } catch (Exception e) {
            log.log(Log.Level.DONGOOFED, e.toString());
        }

        try {
            URL soundURL = Main.class.getResource("resources/hey_ya.wav");
            AudioInputStream audioStream = AudioSystem.getAudioInputStream(soundURL);
            clip.open(audioStream);
            clip.start();
        } catch (Exception e) {
            log.log(Log.Level.DONGOOFED, e.toString());
        }

        if (clip != null) {
            do {
                try {
                    Thread.sleep(50);
                } catch (Exception e) {
                    log.log(Log.Level.DONGOOFED, e.toString());
                }
            } while (clip.isActive());
        }
    }
}
