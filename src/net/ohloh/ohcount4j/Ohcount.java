package net.ohloh.ohcount4j;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import net.ohloh.ohcount4j.AnnotationWriter;
import net.ohloh.ohcount4j.detect.OhcountDetector;
import net.ohloh.ohcount4j.io.FileBlob;
import net.ohloh.ohcount4j.scan.Scanner;

import org.kohsuke.args4j.Argument;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

public class Ohcount {

	public static void main(String[] args) {
		final OhcountOptions opts = new OhcountOptions();
		final CmdLineParser optParser = new CmdLineParser(opts);
		try {
			optParser.parseArgument(args);
		} catch (CmdLineException e) {
			System.err.println("Error parsing options - " + e.getMessage());
			System.exit(-1);
		}

		if (opts.help) {
			optParser.printUsage(System.out);
			System.exit(0);
		}
		if (opts.targets.size() < 1) {
			optParser.printUsage(System.out);
			System.exit(-1);
		}

		try {
			FileFinder ff = new FileFinder();
			for (String path : opts.targets) {
				ff.addPath(path);
			}
			ArrayList<File> files = ff.getFiles();

			if (opts.annotate) {
				annotate(files);
			} else if (opts.detect) {
				detect(files);
			} else {
				summarize(files);
			}
			System.exit(0);
		} catch (OhcountException e) {
			System.err.println("Error - " + e.getMessage());
			System.exit(-1);
		} catch (IOException e) {
			System.err.println("Error - " + e.getMessage());
			System.exit(-1);
		}
	}

	static void annotate(List<File> files) throws IOException, OhcountException {
		AnnotationWriter handler = new AnnotationWriter();
		for (File file : files) {
			FileBlob blob = new FileBlob(file);
			Scanner scanner = OhcountDetector.getInstance().detect(blob);
			if (scanner != null) {
				scanner.scan(blob, handler);
			}
		}
	}

	static void detect(List<File> files) throws IOException, OhcountException {
		for (File file : files) {
			FileBlob blob = new FileBlob(file);
			Scanner scanner = OhcountDetector.getInstance().detect(blob);
			if (scanner != null) {
				System.out.printf("%s\t%s\n", 
					scanner.getLanguage().niceName(), file.getPath());
			}
		}
	}

	static void summarize(List<File> files) throws IOException, OhcountException {
		SummaryWriter summary = new SummaryWriter();
		for (File file : files) {
			FileBlob blob = new FileBlob(file);
			Scanner scanner = OhcountDetector.getInstance().detect(blob);
			if (scanner != null) {
				summary.beginFile();
				scanner.scan(blob, summary);
				summary.endFile();
			}
		}
		summary.printResults();
	}

	static class OhcountOptions {
		@Argument(metaVar = "[file]", usage = "target")
		List<String> targets = new ArrayList<String>();

		@Option(name = "-h", usage = "display this message")
		boolean help = false;

		@Option(name = "-s", usage = "show line count summary (default)")
		boolean summary = true;

		@Option(name = "-a", usage = "show annotated source code")
		boolean annotate = false;

		@Option(name = "-d", usage = "show detected file types only")
		boolean detect = false;
	}

}
