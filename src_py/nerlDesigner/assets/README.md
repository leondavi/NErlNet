# NerlDesigner Logo Setup

## Adding the Logo

To use the 3D network logo in NerlDesigner:

1. Save the 3D molecular/network structure logo image as `logo.png` in this directory:
   ```
   /home/admin/workspace/NErlNet/src_py/nerlDesigner/assets/logo.png
   ```

2. The logo should be in PNG format with the following recommended specifications:
   - **Size**: 256x256 pixels (or similar square dimensions)
   - **Format**: PNG with transparency support
   - **Style**: The 3D molecular/network structure with red and black nodes

3. Once the logo is placed in the correct location, restart NerlDesigner and it will automatically appear in:
   - The header navigation bar
   - The browser favicon
   - The application title

## Fallback Behavior

If no logo file is found, NerlDesigner will use:
- A network tree icon (account_tree) in the header
- A brain emoji (🧠) as the favicon

## Logo Integration

The logo is integrated through the following components:
- **Header**: Displays at 40x40 pixels in the top navigation
- **Favicon**: Uses the logo as the browser tab icon
- **Static Files**: Served via `/assets/` route for web access

## Technical Notes

The logo is loaded dynamically when the application starts:
```python
logo_path = Path(__file__).parent / 'assets' / 'logo.png'
if logo_path.exists():
    ui.image('/assets/logo.png').classes('h-10 w-10')
else:
    ui.icon('account_tree', size='2rem').classes('text-white')
```