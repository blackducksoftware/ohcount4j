package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class PhpScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(Language.PHP, new Line(Language.PHP, BLANK),   "\n");
		assertLine(Language.PHP, new Line(Language.PHP, BLANK),   "     \n");
		assertLine(Language.PHP, new Line(Language.PHP, BLANK),   "\t\n");
		assertLine(Language.PHP, new Line(Language.PHP, CODE),    "$file = fopen('file.txt', 'r+');\n");
		assertLine(Language.PHP, new Line(Language.PHP, COMMENT), "// line comment\n");
		assertLine(Language.PHP, new Line(Language.PHP, COMMENT), "//\n");
		assertLine(Language.PHP, new Line(Language.PHP, COMMENT), "# line comment\n");
		assertLine(Language.PHP, new Line(Language.PHP, COMMENT), "#\n");
		assertLine(Language.PHP, new Line(Language.PHP, CODE),    "$file = fopen('file.txt', 'r+'); // with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(Language.PHP, new Line(Language.PHP, BLANK),   "     ");
		assertLine(Language.PHP, new Line(Language.PHP, BLANK),   "\t");
		assertLine(Language.PHP, new Line(Language.PHP, CODE),    "$file = fopen('file.txt', 'r+');");
		assertLine(Language.PHP, new Line(Language.PHP, COMMENT), "// line comment");
		assertLine(Language.PHP, new Line(Language.PHP, COMMENT), "//");
		assertLine(Language.PHP, new Line(Language.PHP, COMMENT), "# line comment");
		assertLine(Language.PHP, new Line(Language.PHP, COMMENT), "#");
		assertLine(Language.PHP, new Line(Language.PHP, CODE),    "$file = fopen('file.txt', 'r+'); // with comment");
	}

	@Test
	public void sampleTest() {
		String code
			= "/*\n"
			+ "Sample Code Written in PHP\n"
			+ "		For Testing\n"
			+ "*/\n"
			+ "$testHereDoc = <<<HEREDOC\n"
			+ "		#Anything inside this string\n"
			+ "		//Is considered string\n"
			+ "		/* Until the Delimiter is reached */\n"
			+ "\t\n"
			+ "HEREDOC;\n"
			+ "function lock() {\n"
		    + "		$file = fopen('file.txt', 'r+');\n"
		    + "		retry:\n"
		    + "		if (!flock($file, LOCK_EX & LOCK_NB)) {\n"
		    + "    		goto retry;\n"
		    + "		}\n"
		    + "		fwrite($file, 'Success!');\n"
		    + "		fclose($file);\n"
		    + "		return 0;\n"
		    + "}\n";

		Line[] expected = {
				new Line(Language.PHP, COMMENT),
				new Line(Language.PHP, COMMENT),
				new Line(Language.PHP, COMMENT),
				new Line(Language.PHP, COMMENT),
				new Line(Language.PHP, CODE),
				new Line(Language.PHP, CODE),
				new Line(Language.PHP, CODE),
				new Line(Language.PHP, CODE),
				new Line(Language.PHP, BLANK),
				new Line(Language.PHP, CODE),
				new Line(Language.PHP, CODE),
				new Line(Language.PHP, CODE),
				new Line(Language.PHP, CODE),
				new Line(Language.PHP, CODE),
				new Line(Language.PHP, CODE),
				new Line(Language.PHP, CODE),
				new Line(Language.PHP, CODE),
				new Line(Language.PHP, CODE),
				new Line(Language.PHP, CODE),
				new Line(Language.PHP, CODE)
		};
		assertLines(Language.PHP, expected, code);
	}

	@Test
	public void hereDoc() {
		String code
			= "s = <<<HERE_DOC\n"
			+ "This is part of a string\n"
			+ "# this is not a comment\n"
			+ "\t\n"
			+ "HERE_DOC\n"
			+ "		//above statement is not end because no semicolon\n"
			+ "HERE_DOC;\n"
			+ "# this is a comment\n";

		Line[] expected = {
			new Line(Language.PHP, CODE),
			new Line(Language.PHP, CODE),
			new Line(Language.PHP, CODE),
			new Line(Language.PHP, BLANK),
			new Line(Language.PHP, CODE),
			new Line(Language.PHP, CODE),
			new Line(Language.PHP, CODE),
			new Line(Language.PHP, COMMENT)
		};
		assertLines(Language.PHP, expected, code);
	}

	@Test
	public void indentedEndHereDoc() {
		String code
			= "s = <<<HERE_DOC\n"
			+ "This is part of a string\n"
			+ "# this is not a comment\n"
			+ "    HERE_DOC;\n"
			+ "# this is a comment\n";

		Line[] expected = {
			new Line(Language.PHP, CODE),
			new Line(Language.PHP, CODE),
			new Line(Language.PHP, CODE),
			new Line(Language.PHP, CODE),
			new Line(Language.PHP, COMMENT)
		};
		assertLines(Language.PHP, expected, code);
	}

}
