import React from 'react'
import styled from 'styled-components'
import Flex from 'mdx-deck/dist/Flex'
import Box from 'mdx-deck/dist/Box'

const Root = styled.div([], {
  ' img': {
    height: '7vh'
  },
  ' sup': {
    fontSize: '0.65em'
  }
})

export default ({ children }) => {
  const [a, a1, b, b1, c, c1, d, d1] = React.Children.toArray(children.props.children)

  return (
    <Root>
      <Flex
        css={{
          alignItems: 'center',
          flexWrap: 'wrap',
          height: '50%' 
        }}>
        <Box width={1 / 2} style={{marginBottom: '8vh'}}>
          {[a, a1]}
        </Box>
        <Box width={1 / 2} style={{marginBottom: '8vh'}}>
          {[b, b1]}
        </Box>
        <Box width={1 / 2}>
          {[c, c1]}
        </Box>
        <Box width={1 / 2}>
          {[d, d1]}
        </Box>
      </Flex>
    </Root>
  )
}