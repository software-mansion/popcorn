interface IconProps {
  name: string;
  width?: number;
  height?: number;
  className?: string;
}

function Icon({ name, width, height, className }: IconProps) {
  const symbolId = `#icon-icons-${name}`;

  return (
    <svg aria-hidden="true" className={className} width={width} height={height}>
      <use href={symbolId} fill="#000" />
    </svg>
  );
}

export default Icon;
