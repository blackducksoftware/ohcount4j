package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import static net.ohloh.ohcount4j.Language.LANG_JAVA;
import static net.ohloh.ohcount4j.Language.LANG_REBOL;

public class RebolScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new RebolScanner(), new Line(LANG_REBOL, BLANK),   "\n");
		assertLine(new RebolScanner(), new Line(LANG_REBOL, BLANK),   "     \n");
		assertLine(new RebolScanner(), new Line(LANG_REBOL, BLANK),   "\t\n");
		assertLine(new RebolScanner(), new Line(LANG_REBOL, CODE),    "while [not tail? mail] [\n");
		assertLine(new RebolScanner(), new Line(LANG_REBOL, COMMENT), "; Line comment\n");
		assertLine(new RebolScanner(), new Line(LANG_REBOL, COMMENT), ";\n");
		assertLine(new RebolScanner(), new Line(LANG_REBOL, CODE),    "foreach page pages [send boss@hans.dom read page] // with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new RebolScanner(), new Line(LANG_REBOL, BLANK),   "     ");
		assertLine(new RebolScanner(), new Line(LANG_REBOL, BLANK),   "\t");
		assertLine(new RebolScanner(), new Line(LANG_REBOL, CODE),    "while [not tail? mail] [");
		assertLine(new RebolScanner(), new Line(LANG_REBOL, COMMENT), "; Line comment");
		assertLine(new RebolScanner(), new Line(LANG_REBOL, COMMENT), ";");
		assertLine(new RebolScanner(), new Line(LANG_REBOL, CODE),    "foreach page pages [send boss@hans.dom read page] // with comment");
	}

	@Test
	public void sampleTest() {
		String code
			= "; Simple Rebol Code\n"
			+ "view layout [\n"
			+ "		a: area\n" 
			+ "		btn \"Save\" [\n"
			+ "			write %reboltut.txt a/text\n"
			+ "			alert \"Saved\"\n"
			+ "			alert {Random String using\n"
			+ "					curley braces {\n"
			+ "\t\t\n"
			+ "					; comment inside string is string\n"
			+ "					can be nested {\n"
			+ "						many times }}\n"
			+ "				}\n"
			+ "		; comment after string is comment\n"
			+ "		]\n"
			+ "]\n";

		Line[] expected = {
			new Line(LANG_REBOL, COMMENT),
			new Line(LANG_REBOL, CODE),
			new Line(LANG_REBOL, CODE),
			new Line(LANG_REBOL, CODE),
			new Line(LANG_REBOL, CODE),
			new Line(LANG_REBOL, CODE),
			new Line(LANG_REBOL, CODE),
			new Line(LANG_REBOL, CODE),
			new Line(LANG_REBOL, BLANK),
			new Line(LANG_REBOL, CODE),
			new Line(LANG_REBOL, CODE),
			new Line(LANG_REBOL, CODE),
			new Line(LANG_REBOL, CODE),
			new Line(LANG_REBOL, COMMENT),
			new Line(LANG_REBOL, CODE),
			new Line(LANG_REBOL, CODE)
		};
		assertLines(new RebolScanner(), expected, code);
	}
	
	@Test
	public void unterminatedBracketStringCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "{{\nA}\n\n";

		Line[] expected = {
				new Line(LANG_JAVA, CODE),
				new Line(LANG_JAVA, CODE),
				new Line(LANG_JAVA, BLANK)
			};
		assertLines(new JavaScanner(), expected, code);
	}

}