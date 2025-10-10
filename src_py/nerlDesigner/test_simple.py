#!/usr/bin/env python3
"""
Simple test to verify NiceGUI is working properly
"""

from nicegui import ui

def main():
    # Simple test content
    ui.label('🧠 NerlDesigner Test Page').classes('text-h3 text-center mb-4')
    
    with ui.card().classes('w-full max-w-md mx-auto'):
        with ui.card_section():
            ui.label('This is a test page').classes('text-h6')
            ui.label('If you can see this, NiceGUI is working!')
            
            with ui.row().classes('gap-2 mt-4'):
                ui.button('Test Button', on_click=lambda: ui.notify('Button clicked!'))
                ui.button('Another Button', on_click=lambda: ui.notify('Success!'))
    
    # Test tabs
    with ui.tabs() as tabs:
        tab1 = ui.tab('Tab 1')
        tab2 = ui.tab('Tab 2')
    
    with ui.tab_panels(tabs, value=tab1):
        with ui.tab_panel(tab1):
            ui.label('Content of Tab 1')
            ui.input('Test Input', placeholder='Type something...')
            
        with ui.tab_panel(tab2):
            ui.label('Content of Tab 2') 
            ui.switch('Test Switch')

    ui.run(
        title='NerlDesigner Simple Test',
        port=8083,
        host='0.0.0.0',
        favicon='🧠',
        show=True
    )

if __name__ == '__main__':
    main()