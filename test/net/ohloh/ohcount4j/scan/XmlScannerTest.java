package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import static net.ohloh.ohcount4j.Language.*;

public class XmlScannerTest extends BaseScannerTest {
	
	@Test
	public void basic() {
		assertLine(new XmlScanner(), new Line(LANG_XML, BLANK),   "\n");
		assertLine(new XmlScanner(), new Line(LANG_XML, BLANK),   "     \n");
		assertLine(new XmlScanner(), new Line(LANG_XML, BLANK),   "\t\n");
		assertLine(new XmlScanner(), new Line(LANG_XML, CODE),    "<taskdef resource=\"testngtasks\" classpath=\"${lib}/testng-6.3.1.jar\"/>\n");
		assertLine(new XmlScanner(), new Line(LANG_XML, COMMENT), "<!--comment-->\n");
		assertLine(new XmlScanner(), new Line(LANG_XML, CODE),    "<property name=\"lib\" location=\"lib\"/> <!-- with comment -->\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new XmlScanner(), new Line(LANG_XML, BLANK),   "     ");
		assertLine(new XmlScanner(), new Line(LANG_XML, BLANK),   "\t");
		assertLine(new XmlScanner(), new Line(LANG_XML, CODE),    "<taskdef resource=\"testngtasks\" classpath=\"${lib}/testng-6.3.1.jar\"/>");
		assertLine(new XmlScanner(), new Line(LANG_XML, COMMENT), "<!--comment-->");
		assertLine(new XmlScanner(), new Line(LANG_XML, CODE),    "<property name=\"lib\" location=\"lib\"/> <!-- with comment -->");
	}

	@Test
	public void simpleTest() {
		String code = "<path id=\"lib.jars\"\n"
					+ "<!--multi\n"
					+ "\t\n"
					+ "line comment -->\n"
					+ "<fileset dir=\"${lib}\">\n"
					+ "<include name=\"**/*.jar\"/>\n"
					+ "</fileset>\n"
					+ "\n"
					+ "</path>";

		Line[] expected = {
			new Line(LANG_XML, CODE),
			new Line(LANG_XML, COMMENT),
			new Line(LANG_XML, BLANK),
			new Line(LANG_XML, COMMENT),
			new Line(LANG_XML, CODE),
			new Line(LANG_XML, CODE),
			new Line(LANG_XML, CODE),
			new Line(LANG_XML, BLANK),
			new Line(LANG_XML, CODE)
		};
		assertLines(new XmlScanner(), expected, code);
	}
	
	@Test
	public void cdataString() {
		//Everything inside a cdata string should be considered part of the string
		String code = "<!--This is a comment-->\n"
				+ "<![CDATA[\n"
				+ "<!--  this is a comment inside a cdata string-->\n"
				+ "]]>\n"
				+ "<!--<![CDATA[ This is a commented cdata string ]]>-->";
		Line[] expected = {
				new Line(LANG_XML, COMMENT),
				new Line(LANG_XML, CODE),
				new Line(LANG_XML, CODE),
				new Line(LANG_XML, CODE),
				new Line(LANG_XML, COMMENT)
		};
		assertLines(new XmlScanner(), expected, code);
	}
}