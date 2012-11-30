package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class JavaScriptScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, BLANK),   "\n");
		assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, BLANK),   "     \n");
		assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, BLANK),   "\t\n");
		assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, CODE),    "function() {};\n");
		assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, COMMENT), "/* comment */\n");
		assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, CODE),    "function() {}; /* with comment */\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, BLANK),   "     ");
		assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, BLANK),   "\t");
		assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, CODE),    "function() {};");
		assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, COMMENT), "/* comment */");
		assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, CODE),    "function() {}; /* with comment */");
	}

	@Test
	public void escapedCharsInStrings() {
		// A literal newline character embedded within a one-line string should not be
		// incorrectly counted as two lines of code
		assertLine(Language.JAVASCRIPT, new Line(Language.JAVASCRIPT, CODE),
				"var str = \"a newline literal \\n in a string\";");
	}

	@Test
	public void helloWorld() {
		String code
			= "/* Hello World */\n"
			+ "\n"
			+ "$(document).ready(function() {\n"
			+ "\talert(\"Hello, world!\\n\");\n"
			+ "});\n";

		Line[] expected = {
			new Line(Language.JAVASCRIPT, COMMENT),
			new Line(Language.JAVASCRIPT, BLANK),
			new Line(Language.JAVASCRIPT, CODE),
			new Line(Language.JAVASCRIPT, CODE),
			new Line(Language.JAVASCRIPT, CODE)
		};
		assertLines(Language.JAVASCRIPT, expected, code);
	}
}
