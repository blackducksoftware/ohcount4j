package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import static net.ohloh.ohcount4j.Language.LANG_GROOVY;

public class GroovyScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new GroovyScanner(), new Line(LANG_GROOVY, BLANK),   "\n");
		assertLine(new GroovyScanner(), new Line(LANG_GROOVY, BLANK),   "     \n");
		assertLine(new GroovyScanner(), new Line(LANG_GROOVY, BLANK),   "\t\n");
		assertLine(new GroovyScanner(), new Line(LANG_GROOVY, CODE),    "def name='World'; println \"Hello $name!\"\n");
		assertLine(new GroovyScanner(), new Line(LANG_GROOVY, COMMENT), "/* Block Comment */\n");
		assertLine(new GroovyScanner(), new Line(LANG_GROOVY, COMMENT), "// Line comment\n");
		assertLine(new GroovyScanner(), new Line(LANG_GROOVY, COMMENT), "//\n");
		assertLine(new GroovyScanner(), new Line(LANG_GROOVY, CODE),    "def name='World'; println \"Hello $name!\" // with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new GroovyScanner(), new Line(LANG_GROOVY, BLANK),   "     ");
		assertLine(new GroovyScanner(), new Line(LANG_GROOVY, BLANK),   "\t");
		assertLine(new GroovyScanner(), new Line(LANG_GROOVY, CODE),    "def name='World'; println \"Hello $name!\"");
		assertLine(new GroovyScanner(), new Line(LANG_GROOVY, COMMENT), "/* Block Comment */");
		assertLine(new GroovyScanner(), new Line(LANG_GROOVY, COMMENT), "// Line comment");
		assertLine(new GroovyScanner(), new Line(LANG_GROOVY, COMMENT), "//");
		assertLine(new GroovyScanner(), new Line(LANG_GROOVY, CODE),    "def name='World'; println \"Hello $name!\" // with comment");
	}

	@Test
	public void helloWorld() {
		String code
			= "/* Groovy Style\n"
			+ "\n"
			+ "Hello World Program*/\n"
			+ "class Greet {\n"
			+ "	  def name\n"
			+ "	  Greet(who) { name = who[0].toUpperCase() +\n"
			+ "	                      who[1..-1] }\n"
			+ "	  def salute() { println \"Hello $name!\" }\n"
			+ "}\n"
			+ "\n"
			+ "g = new Greet('world')  // create object\n"
			+ "g.salute()              // output \"Hello World!\";\n";



		Line[] expected = {
			new Line(LANG_GROOVY, COMMENT),
			new Line(LANG_GROOVY, BLANK),
			new Line(LANG_GROOVY, COMMENT),
			new Line(LANG_GROOVY, CODE),
			new Line(LANG_GROOVY, CODE),
			new Line(LANG_GROOVY, CODE),
			new Line(LANG_GROOVY, CODE),
			new Line(LANG_GROOVY, CODE),
			new Line(LANG_GROOVY, CODE),
			new Line(LANG_GROOVY, BLANK),
			new Line(LANG_GROOVY, CODE),
			new Line(LANG_GROOVY, CODE)
		};
		assertLines(new GroovyScanner(), expected, code);
	}

}