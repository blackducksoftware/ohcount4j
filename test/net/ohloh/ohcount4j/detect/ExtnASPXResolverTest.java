package net.ohloh.ohcount4j.detect;

import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;
import static org.testng.AssertJUnit.assertEquals;

import java.io.IOException;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.SourceFile;

import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

public class ExtnASPXResolverTest {

	private ExtnASPXResolver r;

	@BeforeTest()
	public void setup() {
		this.r = new ExtnASPXResolver();
	}

	@Test
	public void canResolvetest() {
		assertFalse(r.canResolve(Language.RUBY));
		assertTrue(r.canResolve(Language.ASPX_CSHARP));
		assertTrue(r.canResolve(Language.ASPX_VB));
	}

	@Test
	// With no other clues, the resolver should pick C# by default
	public void returnsCSharpByDefaultTest() throws IOException {
		assertEquals(Language.ASPX_CSHARP, r.resolve(new SourceFile("foo.aspx", "")));
	}

	@Test
	public void csExample() throws IOException {
		assertEquals(Language.ASPX_CSHARP, r.resolve(new SourceFile("foo.aspx",
				"<%@ Page Foo=\"Bar\" Language=\"C#\" %>\n")));

		assertEquals(Language.ASPX_CSHARP, r.resolve(new SourceFile("foo.aspx",
				"<%@Page Language=\"C#\"%>\n")));
	}

	@Test
	public void vbExample() throws IOException {
		assertEquals(Language.ASPX_VB, r.resolve(new SourceFile("foo.aspx",
				"<%@ Page Foo=\"Bar\" Language=\"VB\" %>\n")));

		assertEquals(Language.ASPX_VB, r.resolve(new SourceFile("foo.aspx",
				"<%@ page foo=\"bar\" language=\"vb\" %>\n")));

		assertEquals(Language.ASPX_VB, r.resolve(new SourceFile("foo.aspx",
				"<%@ Page Language=\"VB\" %>\n")));

		assertEquals(Language.ASPX_VB, r.resolve(new SourceFile("foo.aspx",
				"<%@ Page Foo=\"Bar\"  \n    Language=\"VB\" %>\n")));
	}

}
