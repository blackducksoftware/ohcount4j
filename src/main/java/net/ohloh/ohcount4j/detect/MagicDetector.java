package net.ohloh.ohcount4j.detect;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.OhcountException;

public class MagicDetector {

    // Use libmagic to identify the buffer contents.
    // Returns a Scanner class if the file type is recognized, otherwise null.
    public static Language detect(String buffer) {
        String description = getMagicDescription(buffer, MagicForType.BUFFER);
        String languageName = getLanguageName(description);

        return Detector.getInstance().detectByLanguageName(languageName);
    }

    private enum MagicForType {
        BUFFER, FILE_PATH;
    }

    public static Language detectFile(String path) {
        String description = getMagicDescription(path, MagicForType.FILE_PATH);
        String languageName = getLanguageName(description);

        return Detector.getInstance().detectByLanguageName(languageName);
    }

    protected static Pattern patterns[] = {
            Pattern.compile("^script text(?: executable)? for (\\w+)", 0),
            Pattern.compile("(\\w+)(?: -\\w+)* script(?:, \\w+)? text", 0),
            Pattern.compile("(\\w+) program text", 0),
    };

    public static String getLanguageName(String description) {
        if (description == null) {
            return null;
        }

        for (Pattern pattern : patterns) {
            Matcher matcher = pattern.matcher(description);
            if (matcher.find()) {
                return matcher.group(1);
            }
        }
        return null;
    }

    private static String getMagicDescription(String descriptionFor, MagicForType type) {
        if (descriptionFor == null) {
            return null;
        }
        Magic magic = getMagicLib();
        if (magic == null) {
            System.err.println("Can not get magic lib");
            return null;
        }
        try {
            String description;
            if (type == MagicForType.BUFFER) {
                description = magic.buffer(descriptionFor);
            } else {
                description = magic.file(descriptionFor);
            }
            if (magic.error() != null) {
                throw new OhcountException(magic.error());
            }
            return description;
        } finally {
            magic.close();
        }
    }

    /**
     * Opens, loads and returns Magic Lib
     *
     * @return
     */
    private static Magic getMagicLib() {
        Magic magic = new Magic();

        if (!magic.open()) {
            return null;
        }

        if (magic.error() != null) {
            throw new OhcountException(magic.error());
        }

        magic.load();

        if (magic.error() != null) {
            throw new OhcountException(magic.error());
        }

        return magic;
    }

}
