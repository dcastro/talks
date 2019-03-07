import React from 'react'
import styled from 'styled-components'

const Root = styled.div([], {
  ' img': {
    height: '1em'
  }
})

export default ({ children }) => (
  <Root>
    {children}
  </Root>
)
