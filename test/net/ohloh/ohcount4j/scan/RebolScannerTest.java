package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class RebolScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new RebolScanner(), new Line(Language.REBOL, BLANK),   "\n");
		assertLine(new RebolScanner(), new Line(Language.REBOL, BLANK),   "     \n");
		assertLine(new RebolScanner(), new Line(Language.REBOL, BLANK),   "\t\n");
		assertLine(new RebolScanner(), new Line(Language.REBOL, CODE),    "while [not tail? mail] [\n");
		assertLine(new RebolScanner(), new Line(Language.REBOL, COMMENT), "; Line comment\n");
		assertLine(new RebolScanner(), new Line(Language.REBOL, COMMENT), ";\n");
		assertLine(new RebolScanner(), new Line(Language.REBOL, CODE),    "foreach page pages [send boss@hans.dom read page] // with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new RebolScanner(), new Line(Language.REBOL, BLANK),   "     ");
		assertLine(new RebolScanner(), new Line(Language.REBOL, BLANK),   "\t");
		assertLine(new RebolScanner(), new Line(Language.REBOL, CODE),    "while [not tail? mail] [");
		assertLine(new RebolScanner(), new Line(Language.REBOL, COMMENT), "; Line comment");
		assertLine(new RebolScanner(), new Line(Language.REBOL, COMMENT), ";");
		assertLine(new RebolScanner(), new Line(Language.REBOL, CODE),    "foreach page pages [send boss@hans.dom read page] // with comment");
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
			new Line(Language.REBOL, COMMENT),
			new Line(Language.REBOL, CODE),
			new Line(Language.REBOL, CODE),
			new Line(Language.REBOL, CODE),
			new Line(Language.REBOL, CODE),
			new Line(Language.REBOL, CODE),
			new Line(Language.REBOL, CODE),
			new Line(Language.REBOL, CODE),
			new Line(Language.REBOL, BLANK),
			new Line(Language.REBOL, CODE),
			new Line(Language.REBOL, CODE),
			new Line(Language.REBOL, CODE),
			new Line(Language.REBOL, CODE),
			new Line(Language.REBOL, COMMENT),
			new Line(Language.REBOL, CODE),
			new Line(Language.REBOL, CODE)
		};
		assertLines(new RebolScanner(), expected, code);
	}
	
	@Test
	public void unterminatedBracketStringCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "{{\nA}\n\n";

		Line[] expected = {
				new Line(Language.JAVA, CODE),
				new Line(Language.JAVA, CODE),
				new Line(Language.JAVA, BLANK)
			};
		assertLines(new JavaScanner(), expected, code);
	}

}
