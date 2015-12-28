package com.blackducksoftware.ohcount4j;

import com.blackducksoftware.ohcount4j.scan.Line;

// Stores a total count of code, comments, etc. in a single Language
public class LanguageCount implements Comparable<LanguageCount> {
    protected Language language;

    protected int code;

    protected int comment;

    protected int blank;

    protected int fileCount;

    public LanguageCount(Language language) {
        this.language = language;
        code = 0;
        comment = 0;
        blank = 0;
        fileCount = 0;
    }

    public LanguageCount(Language language, int code, int comment, int blank) {
        this.language = language;
        this.code = code;
        this.comment = comment;
        this.blank = blank;
        fileCount = 0;
    }

    public LanguageCount(Line line) {
        language = line.getLanguage();
        code = 0;
        comment = 0;
        blank = 0;
        fileCount = 0;
        add(line);
    }

    public Language getLanguage() {
        return language;
    }

    public int getCode() {
        return code;
    }

    public int getComment() {
        return comment;
    }

    public int getBlank() {
        return blank;
    }

    public int getTotal() {
        return code + comment + blank;
    }

    public float getCommentRatio() {
        if (comment == 0) {
            return 0.0f;
        } else {
            return ((float) comment / (float) (code + comment));
        }
    }

    public LanguageCount add(LanguageCount other) {
        code += other.code;
        comment += other.comment;
        blank += other.blank;
        fileCount += other.fileCount;
        return this;
    }

    public LanguageCount add(Line line) {
        switch (line.getEntity()) {
        case CODE:
            code += 1;
            break;
        case COMMENT:
            comment += 1;
            break;
        case BLANK:
            blank += 1;
            break;
        }
        return this;
    }

    public int incrementFileCount() {
        fileCount++;
        return fileCount;
    }

    public int getFileCount() {
        return fileCount;
    }

    @Override
    public boolean equals(Object object) {
        if (object instanceof LanguageCount) {
            LanguageCount other = (LanguageCount) object;
            return (language == other.language &&
                    code == other.code &&
                    comment == other.comment && blank == other.blank);
        } else {
            return false;
        }
    }

    @Override
    public int compareTo(LanguageCount other) {
        if (getCode() == other.getCode()) {
            if (getComment() == other.getComment()) {
                return new Integer(getBlank()).compareTo(other.getBlank());
            } else {
                return new Integer(getComment()).compareTo(other.getComment());
            }
        } else {
            return new Integer(getCode()).compareTo(other.getCode());
        }
    }
}
