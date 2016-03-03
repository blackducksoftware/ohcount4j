/*
 * Copyright 2016 Black Duck Software, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.blackducksoftware.ohcount4j.detect;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.OhcountConfig;
import com.blackducksoftware.ohcount4j.OhcountException;
import com.blackducksoftware.ohcount4j.SourceFile;
import com.blackducksoftware.ohcount4j.SourceFileUtils;

public class Detector {

    private static final Set<String> BINARY_EXTENSIONS = new HashSet<String>(Arrays.asList("a", "aiff", "au", "avi",
            "bin", "bmp", "cache", "class", "dat", "dll", "doc", "docx", "dylib", "exe", "gif", "gz", "ico", "icns",
            "jar", "jpeg", "jpg", "m4a", "mov", "mp3", "mpg", "ogg", "pdf", "png", "pnt", "ppt", "pptx", "qt", "ra",
            "so", "svg", "svgz", "svn", "swf", "tar", "tgz", "tif", "tiff", "wav", "xls", "xlsx", "xlw", "zip"));

    private static final Detector DETECTOR_INSTANCE = new Detector();

    private final Map<String, Language> nameMap;

    private final Map<String, Language> extensionMap;

    private final Map<String, Language> filenameMap;

    private final Map<String, Class<? extends Resolver>> resolverExtensionMap;

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
            language = EmacsModeDetector.detect(SourceFileUtils.head(source));
        }
        if (language == null) {
            language = getInstance().detectByExtension(source.getExtension(), source, filenames);
        }
        if (language == null) {
            language = getInstance().detectByExtension(source.getExtension().toLowerCase(), source, filenames);
        }
        if (language == null) {
            language = getInstance().detectByFilename(source.getName());
        }

        if (language == null && OhcountConfig.getInstance().useLibmagic()) {
            // give file path to libmagic
            language = MagicDetector.detectFile(source.getPath());
        }

        // A Detector or Resolver may have found a binary file.
        return language != null ? language : Language.UNKNOWN;
    }

    public static Language detect(SourceFile source) throws IOException {
        return detect(source, new ArrayList<String>());
    }

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
    public Language detectByExtension(String ext, SourceFile source, List<String> filenames) throws IOException {
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

    @SuppressWarnings("unchecked")
    public static Resolver getResolver(String ext) {

        // Special case for FORTRAN since it uses so many extensions.
        if (Language.FORTRANFIXED.getExtensions().contains(ext) || Language.FORTRANFREE.getExtensions().contains(ext)) {
            return new FortranResolver();
        }

        String resolverName = Detector.class.getPackage().getName() + ".Extn" + ext.toUpperCase() + "Resolver";
        Class<? extends Resolver> klass;

        try {
            klass = (Class<? extends Resolver>) Class.forName(resolverName);
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

}
