package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class BooScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(Language.BOO, new Line(Language.BOO, BLANK),   "\n");
		assertLine(Language.BOO, new Line(Language.BOO, BLANK),   "     \n");
		assertLine(Language.BOO, new Line(Language.BOO, BLANK),   "\t\n");
		assertLine(Language.BOO, new Line(Language.BOO, CODE),    "break unless i < 10 and i > 5\n");
		assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "/* Block Comment */\n");
		assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "/* /* Nested Block Comment*/ Test */\n");
		assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "// Line comment\n");
		assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "//\n");
		assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "# Line comment\n");
		assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "#\n");
		assertLine(Language.BOO, new Line(Language.BOO, CODE),    "print((0, 'alpha', 4.5, char('d'))) # with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(Language.BOO, new Line(Language.BOO, BLANK),   "     ");
		assertLine(Language.BOO, new Line(Language.BOO, BLANK),   "\t");
		assertLine(Language.BOO, new Line(Language.BOO, CODE),    "break unless i < 10 and i > 5");
		assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "/* Block Comment */");
		assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "/* /* Nested Block Comment*/ Test */");
		assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "// Line comment");
		assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "//");
		assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "# Line comment");
		assertLine(Language.BOO, new Line(Language.BOO, COMMENT), "#");
		assertLine(Language.BOO, new Line(Language.BOO, CODE),    "print((0, 'alpha', 4.5, char('d'))) // with comment");
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
			new Line(Language.BOO, COMMENT),
			new Line(Language.BOO, COMMENT),
			new Line(Language.BOO, BLANK),
			new Line(Language.BOO, COMMENT),
			new Line(Language.BOO, COMMENT),
			new Line(Language.BOO, COMMENT),
			new Line(Language.BOO, COMMENT),
			new Line(Language.BOO, CODE),
			new Line(Language.BOO, CODE),
			new Line(Language.BOO, CODE),
			new Line(Language.BOO, CODE),
			new Line(Language.BOO, CODE),
			new Line(Language.BOO, CODE)
		};
		assertLines(Language.BOO, expected, code);
	}

	@Test
	public void unterminatedNestedCommentCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "/* /*  */\n\n\n";

		Line[] expected = {
				new Line(Language.BOO, COMMENT),
				new Line(Language.BOO, BLANK),
				new Line(Language.BOO, BLANK)
			};
		assertLines(Language.BOO, expected, code);
	}
}
