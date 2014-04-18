package net.ohloh.ohcount4j;

import net.ohloh.ohcount4j.detect.Magic;

public class OhcountConfig {

    private static final String disableLibmagicProperty = "blackduck.ohcount4j.disable.libmagic";

    private static OhcountConfig INSTANCE = new OhcountConfig();

    private boolean disableLibmagic;

    private boolean libmagicExists;

    private OhcountConfig() {
        disableLibmagic = Boolean.valueOf(System.getProperty(disableLibmagicProperty, "true"));
        Magic magic = new Magic();
        libmagicExists = magic.open();

        if (libmagicExists) {
            // We just want to know if it's available
            magic.close();
        }
    }

    public static OhcountConfig getInstance() {
        return INSTANCE;
    }

    public boolean useLibmagic() {
        return !disableLibmagic && libmagicExists;
    }

}
