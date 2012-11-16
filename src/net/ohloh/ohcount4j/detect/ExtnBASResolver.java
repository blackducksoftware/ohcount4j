package net.ohloh.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

import org.apache.commons.io.FilenameUtils;

public class ExtnBASResolver implements Resolver {

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        if (classicBasicPattern.matcher(sourceFile.getCharSequence()).find()) {
            return Language.CLASSIC_BASIC;
        } else if (vbSpecificExtensions(filenames)) {
            return Language.VB;
        } else {
            return Language.STRUCTURED_BASIC;
        }
    }

    @Override
    public Language resolve(SourceFile sourceFile) throws IOException {
        return resolve(sourceFile, new ArrayList<String>());
    }

    @Override
    public boolean canResolve(Language language) {
        if (language == Language.CLASSIC_BASIC ||
                language == Language.STRUCTURED_BASIC ||
                language == Language.VB) {
            return true;
        } else {
            return false;
        }
    }

    // Lines that start with numbers are classic BASIC
    private static Pattern classicBasicPattern = Pattern.compile("^\\s*\\d+");

    private boolean vbSpecificExtensions(List<String> filenames) {
        // The existence of these extensions anywhere in code tree
        // strongly hints that all *.bas files are Visual Basic
        String[] extensions = { "frm", "frx", "vba", "vbp", "vbs" };

        for (String filename : filenames) {
            String extension = FilenameUtils.getExtension(filename);
            for (String vb_ext : extensions) {
                if (extension != null && extension.equals(vb_ext)) {
                    return true;
                }
            }
        }

        return false;
    }
}
