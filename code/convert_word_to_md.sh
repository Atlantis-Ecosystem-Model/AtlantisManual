#!/bin/bash
# Pandoc Word to Markdown Conversion Script
# This script converts Word documents to Markdown with inline images
# Run from the AtlantisWiki root directory
set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${BLUE}=== Pandoc Word to Markdown Conversion ===${NC}"

# Check if we're in the right directory
if [ ! -d "../user_guides/word_sept_2025" ]; then
    echo -e "${RED}Error: ../user_guides/word_sept_2025 directory not found.${NC}"
    exit 1
fi

# Check if pandoc is installed
if ! command -v pandoc &> /dev/null; then
    echo -e "${RED}Error: pandoc is not installed or not in PATH${NC}"
    echo "Please install pandoc from https://pandoc.org/installing.html"
    exit 1
fi

# Define file paths
WORD_FILE="../user_guides/word_sept_2025/AtlantisUserGuide_PartII.docx"
MARKDOWN_DIR="../user_guides/markdown_output"
MARKDOWN_FILE="${MARKDOWN_DIR}/AtlantisUserGuide_PartII.md"

echo -e "${YELLOW}Setting up directories...${NC}"
mkdir -p "$MARKDOWN_DIR"

echo -e "${YELLOW}Converting Word document to Markdown...${NC}"
echo "Input:  $WORD_FILE"
echo "Output: $MARKDOWN_FILE"
echo ""

# Convert Word to Markdown with pandoc
# Using settings for inline images and clean conversion
pandoc "$WORD_FILE" \
    --to markdown \
    --output "$MARKDOWN_FILE" \
    --extract-media="$MARKDOWN_DIR" \
    --wrap=none \
    --markdown-headings=atx \
    --verbose

if [ $? -eq 0 ]; then
    echo ""
    echo -e "${GREEN}Pandoc conversion successful!${NC}"
    echo ""
    echo -e "${BLUE}Created files:${NC}"
    echo "  $MARKDOWN_FILE"
    
    if [ -d "$MARKDOWN_DIR/media" ]; then
        media_count=$(find "$MARKDOWN_DIR/media" -type f | wc -l)
        echo "  $MARKDOWN_DIR/media/ ($media_count files)"
    fi
    
    # Show file size info
    if [ -f "$MARKDOWN_FILE" ]; then
        file_size=$(du -h "$MARKDOWN_FILE" | cut -f1)
        echo "  Markdown file size: $file_size"
    fi
    
    echo ""
    echo -e "${YELLOW}Next steps:${NC}"
    echo "1. Manually fix headers to use # format"
    echo "2. Run the Python splitting script to create QMD chapters"
    echo "3. Preview with 'quarto preview' in quarto_site directory"
    
else
    echo -e "${RED}Pandoc conversion failed${NC}"
    echo "Check the error messages above for details"
    exit 1
fi

echo ""
echo -e "${BLUE}Conversion complete!${NC}"