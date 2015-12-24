package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class RubyScannerTest extends AbstractBaseScannerTest {

	@Test
	public void basic() {
		assertLine(Language.RUBY, new Line(Language.RUBY, BLANK),   "\n");
		assertLine(Language.RUBY, new Line(Language.RUBY, BLANK),   "     \n");
		assertLine(Language.RUBY, new Line(Language.RUBY, BLANK),   "\t\n");
		assertLine(Language.RUBY, new Line(Language.RUBY, CODE),    "require 'lib'\n");
		assertLine(Language.RUBY, new Line(Language.RUBY, COMMENT), "# line comment\n");
		assertLine(Language.RUBY, new Line(Language.RUBY, COMMENT), "#\n");
		assertLine(Language.RUBY, new Line(Language.RUBY, CODE),    "require 'lib' // with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(Language.RUBY, new Line(Language.RUBY, BLANK),   "     ");
		assertLine(Language.RUBY, new Line(Language.RUBY, BLANK),   "\t");
		assertLine(Language.RUBY, new Line(Language.RUBY, CODE),    "require 'lib'");
		assertLine(Language.RUBY, new Line(Language.RUBY, COMMENT), "# line comment");
		assertLine(Language.RUBY, new Line(Language.RUBY, COMMENT), "#");
		assertLine(Language.RUBY, new Line(Language.RUBY, CODE),    "require 'lib' // with comment");
	}

	@Test
	public void helloWorld() {
		String code
			= "# Hello World\n"
			+ "\n"
			+ "puts 'Hello world!'";

		Line[] expected = {
			new Line(Language.RUBY, COMMENT),
			new Line(Language.RUBY, BLANK),
			new Line(Language.RUBY, CODE)
		};
		assertLines(Language.RUBY, expected, code);
	}

	@Test
	public void blockComment() {
		String code
			= "some_code()\n"
			+ "=begin\n"
			+ "This is part of a block comment\n"
			+ "\n"
			+ "A blank line is included above\n"
			+ "=end\n"
			+ "more_code()\n";

		Line[] expected = {
			new Line(Language.RUBY, CODE),
			new Line(Language.RUBY, CODE),
			new Line(Language.RUBY, COMMENT),
			new Line(Language.RUBY, BLANK),
			new Line(Language.RUBY, COMMENT),
			new Line(Language.RUBY, CODE),
			new Line(Language.RUBY, CODE)
		};
		assertLines(Language.RUBY, expected, code);
	}

	@Test
	public void hereDoc() {
		String code
			= "s = <<HERE_DOC\n"
			+ "This is part of a string\n"
			+ "# this is not a comment\n"
			+ "HERE_DOC\n"
			+ "# this is a comment\n";

		Line[] expected = {
			new Line(Language.RUBY, CODE),
			new Line(Language.RUBY, CODE),
			new Line(Language.RUBY, CODE),
			new Line(Language.RUBY, CODE),
			new Line(Language.RUBY, COMMENT)
		};
		assertLines(Language.RUBY, expected, code);
	}

	@Test
	public void indentedHereDoc() {
		String code
			= "s = <<-HERE_DOC\n"
			+ "This is part of a string\n"
			+ "# this is not a comment\n"
			+ "    HERE_DOC\n"
			+ "# this is a comment\n";

		Line[] expected = {
			new Line(Language.RUBY, CODE),
			new Line(Language.RUBY, CODE),
			new Line(Language.RUBY, CODE),
			new Line(Language.RUBY, CODE),
			new Line(Language.RUBY, COMMENT)
		};
		assertLines(Language.RUBY, expected, code);
	}

}
