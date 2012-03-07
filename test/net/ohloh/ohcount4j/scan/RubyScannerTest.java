package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import net.ohloh.ohcount4j.scan.RubyScanner;
import static net.ohloh.ohcount4j.Entity.*;
import static net.ohloh.ohcount4j.Language.*;

public class RubyScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new RubyScanner(), new Line(LANG_RUBY, BLANK),   "\n");
		assertLine(new RubyScanner(), new Line(LANG_RUBY, BLANK),   "     \n");
		assertLine(new RubyScanner(), new Line(LANG_RUBY, BLANK),   "\t\n");
		assertLine(new RubyScanner(), new Line(LANG_RUBY, CODE),    "require 'lib'\n");
		assertLine(new RubyScanner(), new Line(LANG_RUBY, COMMENT), "# line comment\n");
		assertLine(new RubyScanner(), new Line(LANG_RUBY, COMMENT), "#\n");
		assertLine(new RubyScanner(), new Line(LANG_RUBY, CODE),    "require 'lib' // with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new RubyScanner(), new Line(LANG_RUBY, BLANK),   "     ");
		assertLine(new RubyScanner(), new Line(LANG_RUBY, BLANK),   "\t");
		assertLine(new RubyScanner(), new Line(LANG_RUBY, CODE),    "require 'lib'");
		assertLine(new RubyScanner(), new Line(LANG_RUBY, COMMENT), "# line comment");
		assertLine(new RubyScanner(), new Line(LANG_RUBY, COMMENT), "#");
		assertLine(new RubyScanner(), new Line(LANG_RUBY, CODE),    "require 'lib' // with comment");
	}

	@Test
	public void helloWorld() {
		String code
			= "# Hello World\n"
			+ "\n"
			+ "puts 'Hello world!'";

		Line[] expected = {
			new Line(LANG_RUBY, COMMENT),
			new Line(LANG_RUBY, BLANK),
			new Line(LANG_RUBY, CODE)
		};
		assertLines(new RubyScanner(), expected, code);
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
			new Line(LANG_RUBY, CODE),
			new Line(LANG_RUBY, CODE),
			new Line(LANG_RUBY, COMMENT),
			new Line(LANG_RUBY, BLANK),
			new Line(LANG_RUBY, COMMENT),
			new Line(LANG_RUBY, CODE),
			new Line(LANG_RUBY, CODE)
		};
		assertLines(new RubyScanner(), expected, code);
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
			new Line(LANG_RUBY, CODE),
			new Line(LANG_RUBY, CODE),
			new Line(LANG_RUBY, CODE),
			new Line(LANG_RUBY, CODE),
			new Line(LANG_RUBY, COMMENT)
		};
		assertLines(new RubyScanner(), expected, code);
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
			new Line(LANG_RUBY, CODE),
			new Line(LANG_RUBY, CODE),
			new Line(LANG_RUBY, CODE),
			new Line(LANG_RUBY, CODE),
			new Line(LANG_RUBY, COMMENT)
		};
		assertLines(new RubyScanner(), expected, code);
	}

}