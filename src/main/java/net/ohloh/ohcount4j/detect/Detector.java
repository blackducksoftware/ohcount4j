package net.ohloh.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.OhcountConfig;
import net.ohloh.ohcount4j.OhcountException;
import net.ohloh.ohcount4j.SourceFile;

public class Detector {

    // Max length to read content from SourceFile, this should not be very less nor too big
    private static final int MAX_LENGTH = 10000;

    // Min length would be like 100
    private static final int MIN_LENGTH = 100;

    private static final Detector DETECTOR_INSTANCE = new Detector();

    public static Detector getInstance() {
        return DETECTOR_INSTANCE;
    }

    /*
     * Returns the base language of the provided SourceFile. Returns null if no
     * programming language is detected, or if the provided SourceFile is a
     * binary file.
     */
    public static Language detect(SourceFile source, List<String> filenames) throws IOException {

        // Initial fast rejection of binary files
        if (getInstance().isBinary(source.getExtension())) {
            return Language.BINARY;
        }

        Language language = null;

        if (language == null) {
            language = EmacsModeDetector.detect(source.head(MIN_LENGTH));
        }
        if (language == null) {
            language = getInstance().detectByExtension(source.getExtension(),
                    source, filenames);
        }
        if (language == null) {
            language = getInstance().detectByExtension(
                    source.getExtension().toLowerCase(), source, filenames);
        }
        if (language == null) {
            language = getInstance().detectByFilename(source.getName());
        }

        if (language == null && OhcountConfig.getInstance().useLibmagic()) {
            language = MagicDetector.detect(source.head(MAX_LENGTH));
        }

        // A Detector or Resolver may have found a binary file.
        return language != null ? language : Language.UNKNOWN;
    }

    public static Language detect(SourceFile source) throws IOException {
        return detect(source, new ArrayList<String>());
    }

    private final Map<String, Language> nameMap;

    private final Map<String, Language> extensionMap;

    private final Map<String, Language> filenameMap;

    private final Map<String, Class<? extends Resolver>> resolverExtensionMap;

    private Detector() {
        extensionMap = new HashMap<String, Language>();
        filenameMap = new HashMap<String, Language>();
        nameMap = new HashMap<String, Language>();
        resolverExtensionMap = new HashMap<String, Class<? extends Resolver>>();

        for (Language language : Language.values()) {

            nameMap.put(language.uname().toLowerCase(), language);
            nameMap.put(language.niceName().toLowerCase(), language);

            for (String alias : language.getAliases()) {
                nameMap.put(alias.toLowerCase(), language);
            }
            for (String filename : language.getFilenames()) {
                filenameMap.put(filename, language);
            }
            for (String ext : language.getExtensions()) {
                addExtension(ext, language);
            }
        }

        // Remove any ambiguous extensions we've discovered from the
        // fixed extension map, forcing them to be resolved instead.
        for (String ext : resolverExtensionMap.keySet()) {
            extensionMap.remove(ext);
        }

        // Special case for *.in resolver.
        // This Resolver is not associated with any single language; it can (in
        // theory)
        // return any language at all. Thus this resolver will not be
        // "registered" during
        // the language iteration above, and must be manually added.
        resolverExtensionMap.put("in", ExtnINResolver.class);
    }

    private void addExtension(String ext, Language language) {
        Language existing = extensionMap.get(ext);

        if (existing == null) {
            extensionMap.put(ext, language);
        } else {
            /*
             * Collision: two languages, one extension. Confirm that a resolver
             * exists for this extension, and that it can distinguish both of
             * these languages.
             */
            Resolver resolver = getResolver(ext);

            if (resolver.canResolve(existing) && resolver.canResolve(language)) {
                resolverExtensionMap.put(ext, resolver.getClass());
            } else {
                String msg = "File extension conflict: Languages "
                        + language.niceName() + " and " + existing.niceName()
                        + " both use extension '*." + ext + "',"
                        + " but no Resolver exists to distinguish them.";
                throw new OhcountException(msg);
            }
        }
    }

    // Currently assumes extensions map uniquely to scanners
    public Language detectByExtension(String ext, SourceFile source,
            List<String> filenames) throws IOException {
        Class<? extends Resolver> resolverClass = resolverExtensionMap.get(ext);
        if (resolverClass != null) {
            return makeResolver(resolverClass).resolve(source, filenames);
        } else {
            return extensionMap.get(ext);
        }
    }

    public Language detectByFilename(String filename) {
        return filenameMap.get(filename);
    }

    public Language detectByLanguageName(String name) {
        if (name == null) {
            return null;
        }
        return nameMap.get(name.toLowerCase());
    }

    public static Resolver getResolver(String ext) {

        // Special case for FORTRAN since it uses so many extensions.
        if (Language.FORTRANFIXED.getExtensions().contains(ext)
                || Language.FORTRANFREE.getExtensions().contains(ext)) {
            return new FortranResolver();
        }

        String resolverName = "net.ohloh.ohcount4j.detect.Extn"
                + ext.toUpperCase() + "Resolver";

        Class<Resolver> klass;
        try {
            klass = (Class<Resolver>) Class.forName(resolverName);
        } catch (ClassNotFoundException e) {
            throw new OhcountException(e);
        }

        return makeResolver(klass);
    }

    public static Resolver makeResolver(Class<? extends Resolver> resolverClass) {
        try {
            return resolverClass.newInstance();
        } catch (InstantiationException e) {
            throw new OhcountException(e);
        } catch (IllegalAccessException e) {
            throw new OhcountException(e);
        }
    }

    public boolean isBinary(String extension) {
        return BINARY_EXTENSIONS.contains(extension.toLowerCase());
    }

    private static final Set<String> BINARY_EXTENSIONS = new HashSet<String>() {
        {
            add("a");
            add("aiff");
            add("au");
            add("avi");
            add("bin");
            add("bmp");
            add("cache");
            add("class");
            add("dat");
            add("dll");
            add("doc");
            add("docx");
            add("dylib");
            add("exe");
            add("gif");
            add("gz");
            add("ico");
            add("icns");
            add("jar");
            add("jpeg");
            add("jpg");
            add("m4a");
            add("mov");
            add("mp3");
            add("mpg");
            add("ogg");
            add("pdf");
            add("png");
            add("pnt");
            add("ppt");
            add("pptx");
            add("qt");
            add("ra");
            add("so");
            add("svg");
            add("svgz");
            add("svn");
            add("swf");
            add("tar");
            add("tgz");
            add("tif");
            add("tiff");
            add("wav");
            add("xls");
            add("xlsx");
            add("xlw");
            add("zip");
        }
    };

}
