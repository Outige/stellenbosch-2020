import java.io.IOException;
import java.net.URL;

import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.UnsupportedAudioFileException;

public class BasicClipExample {

    public static void main(String[] args) {

        BasicClipExample bc = new BasicClipExample();

        try {
            bc.run();
        } catch (UnsupportedAudioFileException | IOException 
                | LineUnavailableException | InterruptedException e) {
            e.printStackTrace();
        }

    }

    private void run() throws UnsupportedAudioFileException, 
            IOException, LineUnavailableException, InterruptedException
    {
        String filename = "a3.wav";

        URL url = this.getClass().getResource("audio/" + filename);
        System.out.println(url);

        AudioInputStream ais = AudioSystem.getAudioInputStream(url);
        DataLine.Info info = new DataLine.Info(Clip.class, ais.getFormat());
        Clip clip = (Clip) AudioSystem.getLine(info);
        clip.open(ais);
        clip.start();
        Thread.sleep(6000);
        clip.close();
    }       
}