function highlight(line_number)
  local highlighter = {
    CodeBlock = function(block)
      if block.classes:includes('highlight') then
        block.classes:insert('has-line-highlights')
        block.attributes["code-line-numbers"] = line_number
        return block
      end
  end
  }
  return highlighter
end


function Div(el)
  if FORMAT == 'revealjs' then
    if el.classes:includes('cell') then
      line_number = tostring(el.attributes["output-line-numbers"])
      return el:walk(highlight(line_number))
    end
  end
end
