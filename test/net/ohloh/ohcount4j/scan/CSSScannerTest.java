package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import net.ohloh.ohcount4j.scan.CSSScanner;
import static net.ohloh.ohcount4j.Entity.*;
import static net.ohloh.ohcount4j.Language.*;

public class CSSScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new CSSScanner(), new Line(LANG_CSS, BLANK),   "\n");
		assertLine(new CSSScanner(), new Line(LANG_CSS, BLANK),   "     \n");
		assertLine(new CSSScanner(), new Line(LANG_CSS, BLANK),   "\t\n");
		assertLine(new CSSScanner(), new Line(LANG_CSS, CODE),    "margin: 1em;\n");
		assertLine(new CSSScanner(), new Line(LANG_CSS, COMMENT), "/* comment */\n");
		assertLine(new CSSScanner(), new Line(LANG_CSS, CODE),    "margin: 1em; /* with comment */\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new CSSScanner(), new Line(LANG_CSS, BLANK),   "     ");
		assertLine(new CSSScanner(), new Line(LANG_CSS, BLANK),   "\t");
		assertLine(new CSSScanner(), new Line(LANG_CSS, CODE),    "margin: 1em;");
		assertLine(new CSSScanner(), new Line(LANG_CSS, COMMENT), "/* comment */");
		assertLine(new CSSScanner(), new Line(LANG_CSS, CODE),    "margin: 1em; /* with comment */");
	}

	@Test
	public void helloWorld() {
		String code
			= "/* Pure CSS Hello World */\n"
			+ "\n"
			+ "body:after {\n"
			+ "  content:\"Hello, world!\";\n"
			+ "}";

		Line[] expected = {
			new Line(LANG_CSS, COMMENT),
			new Line(LANG_CSS, BLANK),
			new Line(LANG_CSS, CODE),
			new Line(LANG_CSS, CODE),
			new Line(LANG_CSS, CODE)
		};
		assertLines(new CSSScanner(), expected, code);
	}
}