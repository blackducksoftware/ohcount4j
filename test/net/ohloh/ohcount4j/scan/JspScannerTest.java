package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class JspScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(Language.JSP, new Line(Language.JSP, BLANK),   "\n");
		assertLine(Language.JSP, new Line(Language.JSP, BLANK),   "     \n");
		assertLine(Language.JSP, new Line(Language.JSP, BLANK),   "\t\n");
		assertLine(Language.JSP, new Line(Language.JSP, CODE),    "</HTML>\n");
		assertLine(Language.JSP, new Line(Language.JSP, COMMENT), "<!-- comment -->\n");
		assertLine(Language.JSP, new Line(Language.JSP, CODE),    "</HTML><!-- with comment -->\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(Language.JSP, new Line(Language.JSP, BLANK),   "     ");
		assertLine(Language.JSP, new Line(Language.JSP, BLANK),   "\t");
		assertLine(Language.JSP, new Line(Language.JSP, CODE),    "</HTML>");
		assertLine(Language.JSP, new Line(Language.JSP, COMMENT), "<!-- comment -->");
		assertLine(Language.JSP, new Line(Language.JSP, CODE),    "</HTML><!-- with comment -->");
	}

	@Test
	public void helloWorld() {
		String code
			= "<HTML>\n"
			+ "<HTML lang='en'>\n"
			+ "<!-- A comment -->\n"
			+ "<body>\n"
			+ "\n"
			+ "<h1>Hello, world!</h1>\n"
			+ "\n"
			+ "</body>\n"
			+ "</HTML>";

		Line[] expected = {
			new Line(Language.JSP, CODE),
			new Line(Language.JSP, CODE),
			new Line(Language.JSP, COMMENT),
			new Line(Language.JSP, CODE),
			new Line(Language.JSP, BLANK),
			new Line(Language.JSP, CODE),
			new Line(Language.JSP, BLANK),
			new Line(Language.JSP, CODE),
			new Line(Language.JSP, CODE)
		};
		assertLines(Language.JSP, expected, code);
	}

	@Test
	public void embeddedCSSOnSeparateLine() {
		String code
			= "<HTML>\n"
			+ "<style>\n"
			+ "  body:after { content:\"Hello, world!\"; }\n"
			+ "</style>\n"
			+ "</HTML>";

		Line[] expected = {
			new Line(Language.JSP, CODE),
			new Line(Language.JSP, CODE),
			new Line(Language.CSS, CODE),
			new Line(Language.JSP, CODE),
			new Line(Language.JSP, CODE)
		};
		assertLines(Language.JSP, expected, code);
	}

	@Test
	public void embeddedCSSOnSameLine() {
		String code
			= "<HTML>\n"
			+ "<style> body:after { content:\"Hello, world!\"; } </style>\n"
			+ "</HTML>";

		Line[] expected = {
			new Line(Language.JSP, CODE),
			new Line(Language.CSS, CODE),
			new Line(Language.JSP, CODE)
		};
		assertLines(Language.JSP, expected, code);
	}

	@Test
	public void emptyCSSOnSameLine() {
		String code
			= "<HTML>\n"
			+ "<style></style>\n"
			+ "</HTML>";

		Line[] expected = {
			new Line(Language.JSP, CODE),
			new Line(Language.JSP, CODE),
			new Line(Language.JSP, CODE)
		};
		assertLines(Language.JSP, expected, code);
	}

	@Test
	public void commentCSSOnSameLine() {
		String code
			= "<HTML>\n"
			+ "<style>/* No code just comment */</style>\n"
			+ "</HTML>";

		Line[] expected = {
			new Line(Language.JSP, CODE),
			new Line(Language.CSS, COMMENT),
			new Line(Language.JSP, CODE)
		};
		assertLines(Language.JSP, expected, code);
	}

	@Test
	public void embeddedJavaScriptOnSeparateLine() {
		String code
			= "<HTML>\n"
			+ "<script type=\"script/javascript\">\n"
			+ "  document.write(\"Hello, world!\\n\");\n"
			+ "</script>\n"
			+ "</HTML>";

		Line[] expected = {
			new Line(Language.JSP, CODE),
			new Line(Language.JSP, CODE),
			new Line(Language.JAVASCRIPT, CODE),
			new Line(Language.JSP, CODE),
			new Line(Language.JSP, CODE)
		};
		assertLines(Language.JSP, expected, code);
	}

	@Test
	public void embeddedJavaScriptOnSameLine() {
		String code
			= "<HTML>\n"
			+ "<script type=\"script/javascript\">document.write(\"Hello, world!\\n\");</script>\n"
			+ "</HTML>";

		Line[] expected = {
			new Line(Language.JSP, CODE),
			new Line(Language.JAVASCRIPT, CODE),
			new Line(Language.JSP, CODE)
		};
		assertLines(Language.JSP, expected, code);
	}
	
	@Test
	public void embeddedJavaOnSeparateLine() {
		String code
			= "<HTML>\n"
			+ "<%!\n"
			+ "  String message = \"Hello, World, from JSP\";\n"
			+ "%>\n"
			+ "</HTML>";

		Line[] expected = {
			new Line(Language.JSP, CODE),
			new Line(Language.JSP, CODE),
			new Line(Language.JAVA, CODE),
			new Line(Language.JSP, CODE),
			new Line(Language.JSP, CODE)
		};
		assertLines(Language.JSP, expected, code);
	}

	@Test
	public void embeddedJavaOnSameLine() {
		String code
			= "<HTML>\n"
			+ "<% new java.util.Date() %>\n"
			+ "</HTML>";

		Line[] expected = {
			new Line(Language.JSP, CODE),
			new Line(Language.JAVA, CODE),
			new Line(Language.JSP, CODE)
		};
		assertLines(Language.JSP, expected, code);
	}
	
	@Test
	public void commentJavaOnSameLine() {
		String code
			= "<HTML>\n"
			+ "<% /* No code just comment */ %>\n"
			+ "</HTML>";

		Line[] expected = {
			new Line(Language.JSP, CODE),
			new Line(Language.JAVA, COMMENT),
			new Line(Language.JSP, CODE)
		};
		assertLines(Language.JSP, expected, code);
	}

}