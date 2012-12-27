package net.ohloh.ohcount4j;

import net.ohloh.ohcount4j.detect.Magic;
import net.ohloh.ohcount4j.detect.MagicDetector;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class OhcountConfig {

    private static final Logger logger = LoggerFactory.getLogger(MagicDetector.class);

    private static final String useLibmagicProperty = "net.ohloh.ohcount4j.uselibmagic";

    private static OhcountConfig INSTANCE = new OhcountConfig();

    private boolean useLibmagic;

    private boolean libmagicExists;

    private OhcountConfig() {
        useLibmagic = Boolean.valueOf(System.getProperty(useLibmagicProperty, "true"));
        try {
            Magic magic = new Magic();
            magic.open();
            magic.close();
            libmagicExists = true;
        } catch (UnsatisfiedLinkError e) {
            libmagicExists = false;
            logger.warn(e.getMessage());
        }
    }

    public static OhcountConfig getInstance() {
        return INSTANCE;
    }

    public boolean useLibmagic() {
        return useLibmagic && libmagicExists;
    }

}
