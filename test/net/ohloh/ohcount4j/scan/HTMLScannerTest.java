package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import net.ohloh.ohcount4j.scan.HTMLScanner;
import static net.ohloh.ohcount4j.Entity.*;
import static net.ohloh.ohcount4j.Language.*;

public class HTMLScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new HTMLScanner(), new Line(LANG_HTML, BLANK),   "\n");
		assertLine(new HTMLScanner(), new Line(LANG_HTML, BLANK),   "     \n");
		assertLine(new HTMLScanner(), new Line(LANG_HTML, BLANK),   "\t\n");
		assertLine(new HTMLScanner(), new Line(LANG_HTML, CODE),    "<html>\n");
		assertLine(new HTMLScanner(), new Line(LANG_HTML, COMMENT), "<!-- comment -->\n");
		assertLine(new HTMLScanner(), new Line(LANG_HTML, CODE),    "<html><!-- with comment -->\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new HTMLScanner(), new Line(LANG_HTML, BLANK),   "     ");
		assertLine(new HTMLScanner(), new Line(LANG_HTML, BLANK),   "\t");
		assertLine(new HTMLScanner(), new Line(LANG_HTML, CODE),    "<html>");
		assertLine(new HTMLScanner(), new Line(LANG_HTML, COMMENT), "<!-- comment -->");
		assertLine(new HTMLScanner(), new Line(LANG_HTML, CODE),    "<html><!-- with comment -->");
	}

	@Test
	public void helloWorld() {
		String code
			= "<!doctype HTML>\n"
			+ "<html lang='en'>\n"
			+ "<!-- A comment -->\n"
			+ "<body>\n"
			+ "\n"
			+ "<h1>Hello, world!</h1>\n"
			+ "\n"
			+ "</body>\n"
			+ "<html>";

		Line[] expected = {
			new Line(LANG_HTML, CODE),
			new Line(LANG_HTML, CODE),
			new Line(LANG_HTML, COMMENT),
			new Line(LANG_HTML, CODE),
			new Line(LANG_HTML, BLANK),
			new Line(LANG_HTML, CODE),
			new Line(LANG_HTML, BLANK),
			new Line(LANG_HTML, CODE),
			new Line(LANG_HTML, CODE)
		};
		assertLines(new HTMLScanner(), expected, code);
	}
}