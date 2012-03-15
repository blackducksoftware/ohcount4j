package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import net.ohloh.ohcount4j.scan.CSSScanner;
import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class CSSScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new CSSScanner(), new Line(Language.CSS, BLANK),   "\n");
		assertLine(new CSSScanner(), new Line(Language.CSS, BLANK),   "     \n");
		assertLine(new CSSScanner(), new Line(Language.CSS, BLANK),   "\t\n");
		assertLine(new CSSScanner(), new Line(Language.CSS, CODE),    "margin: 1em;\n");
		assertLine(new CSSScanner(), new Line(Language.CSS, COMMENT), "/* comment */\n");
		assertLine(new CSSScanner(), new Line(Language.CSS, CODE),    "margin: 1em; /* with comment */\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new CSSScanner(), new Line(Language.CSS, BLANK),   "     ");
		assertLine(new CSSScanner(), new Line(Language.CSS, BLANK),   "\t");
		assertLine(new CSSScanner(), new Line(Language.CSS, CODE),    "margin: 1em;");
		assertLine(new CSSScanner(), new Line(Language.CSS, COMMENT), "/* comment */");
		assertLine(new CSSScanner(), new Line(Language.CSS, CODE),    "margin: 1em; /* with comment */");
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
			new Line(Language.CSS, COMMENT),
			new Line(Language.CSS, BLANK),
			new Line(Language.CSS, CODE),
			new Line(Language.CSS, CODE),
			new Line(Language.CSS, CODE)
		};
		assertLines(new CSSScanner(), expected, code);
	}
}
