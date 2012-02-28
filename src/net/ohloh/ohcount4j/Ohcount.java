package net.ohloh.ohcount4j;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import net.ohloh.ohcount4j.AnnotationWriter;
import net.ohloh.ohcount4j.detect.SimpleDetector;
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
			if (opts.annotate) {
				annotate(opts.targets);
			} else {
				summarize(opts.targets);
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

	static void annotate(List<String> targets) throws OhcountException, IOException {
		AnnotationWriter handler = new AnnotationWriter();
		for (String filename : targets) {
			FileBlob blob = new FileBlob(new File(filename));
			Scanner scanner = SimpleDetector.detect(blob);
			if (scanner != null) {
				scanner.scan(blob, handler);
			}
		}
	}

	static void summarize(List<String> targets) throws OhcountException, IOException {
		SummaryWriter handler = new SummaryWriter();
		DirectoryScanner ds = new DirectoryScanner(handler);
		for (String filename : targets) {
			ds.scan(new File(filename));
		}
		handler.printResults();
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
	}

}
