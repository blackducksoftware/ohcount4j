package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import static net.ohloh.ohcount4j.Language.LANG_BOO;

public class BooScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new BooScanner(), new Line(LANG_BOO, BLANK),   "\n");
		assertLine(new BooScanner(), new Line(LANG_BOO, BLANK),   "     \n");
		assertLine(new BooScanner(), new Line(LANG_BOO, BLANK),   "\t\n");
		assertLine(new BooScanner(), new Line(LANG_BOO, CODE),    "break unless i < 10 and i > 5\n");
		assertLine(new BooScanner(), new Line(LANG_BOO, COMMENT), "/* Block Comment */\n");
		assertLine(new BooScanner(), new Line(LANG_BOO, COMMENT), "/* /* Nested Block Comment*/ Test */\n");
		assertLine(new BooScanner(), new Line(LANG_BOO, COMMENT), "// Line comment\n");
		assertLine(new BooScanner(), new Line(LANG_BOO, COMMENT), "//\n");
		assertLine(new BooScanner(), new Line(LANG_BOO, COMMENT), "# Line comment\n");
		assertLine(new BooScanner(), new Line(LANG_BOO, COMMENT), "#\n");
		assertLine(new BooScanner(), new Line(LANG_BOO, CODE),    "print((0, 'alpha', 4.5, char('d'))) # with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new BooScanner(), new Line(LANG_BOO, BLANK),   "     ");
		assertLine(new BooScanner(), new Line(LANG_BOO, BLANK),   "\t");
		assertLine(new BooScanner(), new Line(LANG_BOO, CODE),    "break unless i < 10 and i > 5");
		assertLine(new BooScanner(), new Line(LANG_BOO, COMMENT), "/* Block Comment */");
		assertLine(new BooScanner(), new Line(LANG_BOO, COMMENT), "/* /* Nested Block Comment*/ Test */");
		assertLine(new BooScanner(), new Line(LANG_BOO, COMMENT), "// Line comment");
		assertLine(new BooScanner(), new Line(LANG_BOO, COMMENT), "//");
		assertLine(new BooScanner(), new Line(LANG_BOO, COMMENT), "# Line comment");
		assertLine(new BooScanner(), new Line(LANG_BOO, COMMENT), "#");
		assertLine(new BooScanner(), new Line(LANG_BOO, CODE),    "print((0, 'alpha', 4.5, char('d'))) // with comment");
	}

	@Test
	public void sampleTest() {
		String code
			= "# Sample Program Written in Boo\n"
			+ "/* Code For Simple Use\n"
			+ "\n"
			+ "		of an array /*\n"
			+ "			This comment has a nested /*\n"
			+ "			Comment inside of it */ */\n"
			+ "*/\n"
			+ "print((0, 'alpha', 4.5, char('d')))\n"
			+ "print array('abcdefghij') // Print the array\n"
			+ "l = array(range(5))\n"
			+ "print l\n"
			+ "l[2] = 5\n"
			+ "print l\n";

		Line[] expected = {
			new Line(LANG_BOO, COMMENT),
			new Line(LANG_BOO, COMMENT),
			new Line(LANG_BOO, BLANK),
			new Line(LANG_BOO, COMMENT),
			new Line(LANG_BOO, COMMENT),
			new Line(LANG_BOO, COMMENT),
			new Line(LANG_BOO, COMMENT),
			new Line(LANG_BOO, CODE),
			new Line(LANG_BOO, CODE),
			new Line(LANG_BOO, CODE),
			new Line(LANG_BOO, CODE),
			new Line(LANG_BOO, CODE),
			new Line(LANG_BOO, CODE)
		};
		assertLines(new BooScanner(), expected, code);
	}

	@Test
	public void unterminatedNestedCommentCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "/* /*  */\n\n\n";

		Line[] expected = {
				new Line(LANG_BOO, COMMENT),
				new Line(LANG_BOO, BLANK),
				new Line(LANG_BOO, BLANK)
			};
		assertLines(new BooScanner(), expected, code);
	}
}