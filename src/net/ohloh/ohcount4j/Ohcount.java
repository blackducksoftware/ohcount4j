package net.ohloh.ohcount4j;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import net.ohloh.ohcount4j.detect.OhcountDetector;
import net.ohloh.ohcount4j.io.FileBlob;
import net.ohloh.ohcount4j.scan.Line;
import net.ohloh.ohcount4j.scan.LineEventHandler;
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

		if (!opts.annotate) {
			// FIXME - show usage when opts.targets.size() <= 0
			optParser.printUsage(System.out);
			System.exit(-2);
		}

		String fileName = "test/data/input.c"; // FIXME - remove this, testing
												// only.
		if (opts.targets.size() > 0) {
			fileName = opts.targets.get(0);
		}

		try {
			FileBlob blob = new FileBlob(new File(fileName));
			Scanner scanner = OhcountDetector.getInstance().detect(blob);
			LineEventHandler eh = new LineEventHandler();
			scanner.scan(blob, eh);
			for (Line line : eh.getLines()) {
				System.out.print(line);
			}
			System.out.print("\n");
			System.exit(0);
		} catch (OhcountException e) {
			System.err.println("Error - " + e.getMessage());
			System.exit(-1);
		} catch (IOException e) {
			System.err.println("Error - " + e.getMessage());
			System.exit(-1);
		}

	}

	static class OhcountOptions {

		@Argument(metaVar = "[file]", usage = "target")
		List<String> targets = new ArrayList<String>();

		@Option(name = "-h", usage = "display this message")
		boolean help = false;

		@Option(name = "-a", usage = "show annotated source code")
		boolean annotate = false;

	}

}
