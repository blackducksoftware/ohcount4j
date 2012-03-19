package net.ohloh.ohcount4j.detect;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.OhcountException;
import net.ohloh.ohcount4j.io.Source;

public class Detector {

	public static Language detect(Source source) throws IOException {

		if (isBinary(source.getExtension())) {
			return null;
		}

		Language language = null;

		if (language == null) {
			language = EmacsModeDetector.detect(source.head(100));
		}
		if (language == null) {
			language = detectByExtension(source.getExtension(), source);
		}
		if (language == null) {
			language = detectByExtension(source.getExtension().toLowerCase(), source);
		}
		if (language == null) {
			language = detectByFilename(source.getName());
		}
		if (language == null) {
			language = MagicDetector.detect(source.head(100));
		}
		return language;
	}

	private static Map<String, Language> nameMap;
	private static Map<String, Language> extensionMap;
	private static Map<String, Language> filenameMap;
	private static Map<String, Resolver> resolverExtensionMap;

	private static void initialize() {
		extensionMap = new HashMap<String, Language>();
		filenameMap = new HashMap<String, Language>();
		nameMap = new HashMap<String, Language>();
		resolverExtensionMap = new HashMap<String, Resolver>();

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
	}

	private static void addExtension(String ext, Language language) {
		Language existing = extensionMap.get(ext);

		if (existing == null) {
			extensionMap.put(ext, language);

		} else {
			/* Collision: two languages, one extension.
			 * Confirm that a resolver exists for this extension,
			 * and that it can distinguish both of these languages.
			 */
			Resolver resolver = getResolver(ext);

			if (resolver.canResolve(existing) && resolver.canResolve(language)) {
				resolverExtensionMap.put(ext, resolver);
			} else {
				String msg = "File extension conflict: Languages " +
						language.niceName() + " and " + existing.niceName() +
						" both use extension '*." + ext + "'," +
						" but no Resolver exists to distinguish them.";
				throw new OhcountException(msg);
			}
		}
	}

	// Currently assumes extensions map uniquely to scanners
	public static Language detectByExtension(String ext, Source source) throws IOException {
		if (extensionMap == null) {
			initialize();
		}
		Resolver resolver = resolverExtensionMap.get(ext);
		if (resolver != null) {
			return resolver.resolve(source);
		} else {
			return extensionMap.get(ext);
		}
	}

	public static Language detectByFilename(String filename) {
		if (filenameMap == null) {
			initialize();
		}
		return filenameMap.get(filename);
	}

	public static Language detectByLanguageName(String name) {
		if (name == null) {
			return null;
		}
		if (nameMap == null) {
			initialize();
		}
		return nameMap.get(name.toLowerCase());
	}

	public static Resolver getResolver(String ext) {
		String resolverName =
			"net.ohloh.ohcount4j.detect.Extn" + ext.toUpperCase() + "Resolver";

		Class<Resolver> klass;
		try {
			klass = (Class<Resolver>) Class.forName(resolverName);
		} catch (ClassNotFoundException e) {
			throw new OhcountException(e);
		}

		try {
			return klass.newInstance();
		} catch (InstantiationException e) {
			throw new OhcountException(e);
		} catch (IllegalAccessException e) {
			throw new OhcountException(e);
		}
	}

	public static boolean isBinary(String extension) {
		return binaryExtensions.contains(extension.toLowerCase());
	}

	@SuppressWarnings("serial")
	private static Set<String> binaryExtensions = new HashSet<String>() {{
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
	}};

}